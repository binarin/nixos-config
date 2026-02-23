"""Nix command runner with common options support."""

import os
import shutil
import subprocess
import sys
from dataclasses import dataclass, field
from pathlib import Path
from typing import Optional

from rich.console import Console

from . import config
from .external import ExternalToolError

console = Console()


@dataclass
class NixRunner:
    """Central nix command executor with common options.

    Supports:
    - Verbosity control (-q, -v, -vv)
    - Optional nix-output-monitor (nom) integration
    - Optional remote builders
    - Common options like --keep-going, -j
    """

    verbosity: int = 1  # 0=quiet, 1=normal, 2=verbose
    use_nom: Optional[bool] = None  # None means auto-detect
    builders: list[str] = field(default_factory=list)
    keep_going: bool = True
    jobs: str = "auto"
    repo_root: Optional[Path] = None

    def __post_init__(self):
        if self.repo_root is None:
            self.repo_root = config.find_repo_root()
        if self.use_nom is None:
            self.use_nom = self._should_use_nom()

    def _should_use_nom(self) -> bool:
        """Auto-detect whether to use nom.

        Use nom when:
        - stdout is a TTY
        - nom is available in PATH
        """
        if not sys.stdout.isatty():
            return False
        return shutil.which("nom") is not None

    def _get_common_args(self) -> list[str]:
        """Get common nix arguments based on settings."""
        args = []

        # Verbosity
        if self.verbosity == 0:
            args.append("--quiet")
        elif self.verbosity >= 2:
            args.append("-v")
            if self.verbosity >= 3:
                args.append("-v")

        return args

    def _get_build_args(self) -> list[str]:
        """Get arguments specific to build operations."""
        args = self._get_common_args()

        if self.keep_going:
            args.append("--keep-going")

        args.extend(["-j", self.jobs])

        # Remote builders
        if self.builders:
            builders_str = " ".join(self.builders)
            args.extend(["--builders", builders_str, "--max-jobs", "0"])

        return args

    def run_build(
        self,
        flake_ref: str,
        output: Optional[Path] = None,
        impure: bool = False,
        extra_args: Optional[list[str]] = None,
        env: Optional[dict[str, str]] = None,
    ) -> subprocess.CompletedProcess:
        """Execute nix build with common options.

        Args:
            flake_ref: The flake reference to build (e.g., ".#package")
            output: Optional output path (-o)
            impure: Enable impure mode
            extra_args: Additional arguments to pass to nix build
            env: Additional environment variables

        Returns:
            CompletedProcess result
        """
        cmd = ["nix", "build", flake_ref]
        cmd.extend(self._get_build_args())

        if output is not None:
            cmd.extend(["-o", str(output)])

        if impure:
            cmd.append("--impure")

        if extra_args:
            cmd.extend(extra_args)

        return self._run_nix_command(cmd, env=env)

    def run_eval(
        self,
        flake_ref: str,
        raw: bool = False,
        json_output: bool = False,
        apply: Optional[str] = None,
        extra_args: Optional[list[str]] = None,
    ) -> subprocess.CompletedProcess:
        """Execute nix eval with common options.

        Args:
            flake_ref: The flake reference to evaluate
            raw: Output raw string (--raw)
            json_output: Output as JSON (--json)
            apply: Apply a function to the result (--apply)
            extra_args: Additional arguments

        Returns:
            CompletedProcess result
        """
        cmd = ["nix", "eval", flake_ref]
        cmd.extend(self._get_common_args())

        if raw:
            cmd.append("--raw")
        if json_output:
            cmd.append("--json")
        if apply:
            cmd.extend(["--apply", apply])

        if extra_args:
            cmd.extend(extra_args)

        # eval doesn't use nom
        return self._run_nix_command(cmd, use_nom=False)

    def _run_nix_command(
        self,
        cmd: list[str],
        env: Optional[dict[str, str]] = None,
        use_nom: Optional[bool] = None,
    ) -> subprocess.CompletedProcess:
        """Run a nix command, optionally through nom.

        Args:
            cmd: The command to run
            env: Additional environment variables
            use_nom: Override nom usage (None means use self.use_nom)
        """
        full_env = os.environ.copy()
        if env:
            full_env.update(env)

        should_use_nom = use_nom if use_nom is not None else self.use_nom

        # Only use nom for build commands
        if should_use_nom and cmd[1] == "build":
            # Pipe nix build through nom
            return self._run_with_nom(cmd, full_env)
        else:
            return self._run_direct(cmd, full_env)

    def _run_direct(
        self, cmd: list[str], env: dict[str, str]
    ) -> subprocess.CompletedProcess:
        """Run command directly without nom."""
        try:
            result = subprocess.run(
                cmd,
                cwd=self.repo_root,
                capture_output=True,
                text=True,
                env=env,
            )
            if result.returncode != 0:
                raise ExternalToolError(
                    cmd[0], result.stderr.strip(), result.returncode
                )
            return result
        except FileNotFoundError:
            raise ExternalToolError(cmd[0], f"Command not found: {cmd[0]}")

    def _run_with_nom(
        self, cmd: list[str], env: dict[str, str]
    ) -> subprocess.CompletedProcess:
        """Run nix build piped through nom."""
        # For nom, we need to run nix build and pipe its output to nom
        # nom reads the build log from stdin
        try:
            nix_proc = subprocess.Popen(
                cmd,
                cwd=self.repo_root,
                stdout=subprocess.PIPE,
                stderr=subprocess.STDOUT,
                env=env,
            )

            nom_proc = subprocess.Popen(
                ["nom"],
                stdin=nix_proc.stdout,
                cwd=self.repo_root,
            )

            # Allow nix_proc to receive SIGPIPE if nom exits
            if nix_proc.stdout:
                nix_proc.stdout.close()

            nom_proc.wait()
            nix_returncode = nix_proc.wait()

            if nix_returncode != 0:
                raise ExternalToolError(
                    cmd[0],
                    f"nix build failed with exit code {nix_returncode}",
                    nix_returncode,
                )

            return subprocess.CompletedProcess(
                args=cmd, returncode=0, stdout="", stderr=""
            )
        except FileNotFoundError as e:
            if "nom" in str(e):
                # Fall back to direct execution if nom not found
                console.print("[yellow]nom not found, running without it[/yellow]")
                return self._run_direct(cmd, env)
            raise ExternalToolError(cmd[0], f"Command not found: {cmd[0]}")

    def _run_streaming(
        self, cmd: list[str], env: dict[str, str]
    ) -> subprocess.CompletedProcess:
        """Run command with output streaming to console."""
        try:
            process = subprocess.Popen(
                cmd,
                cwd=self.repo_root,
                stdout=subprocess.PIPE,
                stderr=subprocess.STDOUT,
                text=True,
                env=env,
            )

            output_lines = []
            while True:
                line = process.stdout.readline() if process.stdout else None
                if not line and process.poll() is not None:
                    break
                if line:
                    print(line, end="")
                    output_lines.append(line)

            returncode = process.wait()
            output = "".join(output_lines)

            if returncode != 0:
                raise ExternalToolError(cmd[0], output.strip(), returncode)

            return subprocess.CompletedProcess(
                args=cmd, returncode=0, stdout=output, stderr=""
            )
        except FileNotFoundError:
            raise ExternalToolError(cmd[0], f"Command not found: {cmd[0]}")


def get_nixos_configurations(runner: Optional[NixRunner] = None) -> list[str]:
    """Get list of all nixosConfigurations from the flake."""
    import json

    if runner is None:
        runner = NixRunner()

    result = runner.run_eval(
        ".#nixosConfigurations",
        json_output=True,
        apply="builtins.attrNames",
    )
    return json.loads(result.stdout)
