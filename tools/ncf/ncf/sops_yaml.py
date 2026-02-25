"""SOPS YAML configuration manipulation."""

from pathlib import Path
from typing import Optional

from ruamel.yaml import YAML
from ruamel.yaml.comments import CommentedMap, CommentedSeq

from . import config
from . import external


class SopsYaml:
    """Manipulate .sops.yaml file while preserving formatting and anchors."""

    def __init__(self, path: Optional[Path] = None):
        self.path = path or config.get_sops_yaml_path()
        self.yaml = YAML()
        self.yaml.preserve_quotes = True
        # Use wide width to prevent line breaks in the middle of anchors/values
        # The formatting will be normalized by yamlfmt after save
        self.yaml.width = 4096
        self._data: Optional[CommentedMap] = None

    def load(self) -> CommentedMap:
        """Load the .sops.yaml file."""
        with open(self.path, "r") as f:
            self._data = self.yaml.load(f)
        return self._data

    def save(self) -> None:
        """Save the .sops.yaml file, preserving formatting."""
        if self._data is None:
            raise RuntimeError("No data loaded, call load() first")
        with open(self.path, "w") as f:
            self.yaml.dump(self._data, f)
        # Run yamlfmt to ensure consistent formatting that passes nix fmt
        external.yamlfmt(self.path)

    @property
    def data(self) -> CommentedMap:
        """Get the loaded data, loading if necessary."""
        if self._data is None:
            self.load()
        return self._data

    def get_keys_section(self) -> CommentedSeq:
        """Get the keys section of .sops.yaml."""
        if "keys" not in self.data:
            self.data["keys"] = CommentedSeq()
        return self.data["keys"]

    def get_creation_rules(self) -> CommentedSeq:
        """Get the creation_rules section of .sops.yaml."""
        if "creation_rules" not in self.data:
            self.data["creation_rules"] = CommentedSeq()
        return self.data["creation_rules"]

    def find_key_anchor(self, anchor_name: str) -> Optional[int]:
        """Find the index of a key with the given anchor name."""
        keys = self.get_keys_section()
        for i, key in enumerate(keys):
            if hasattr(key, "anchor") and key.anchor.value == anchor_name:
                return i
        return None

    def add_key_anchor(self, anchor_name: str, key_value: str) -> bool:
        """Add a key anchor to the keys section.

        Returns True if the key was added, False if it already exists.
        """
        keys = self.get_keys_section()

        # Check if anchor already exists
        if self.find_key_anchor(anchor_name) is not None:
            return False

        # Create a new scalar with an anchor
        from ruamel.yaml.scalarstring import PlainScalarString

        key_scalar = PlainScalarString(key_value)
        key_scalar.yaml_set_anchor(anchor_name)

        keys.append(key_scalar)
        return True

    def find_creation_rule(self, path_regex: str) -> Optional[int]:
        """Find the index of a creation rule with the given path_regex."""
        rules = self.get_creation_rules()
        for i, rule in enumerate(rules):
            if isinstance(rule, dict) and rule.get("path_regex") == path_regex:
                return i
        return None

    def add_creation_rule(
        self,
        path_regex: str,
        pgp_keys: list[str],
        age_keys: list[str],
    ) -> bool:
        """Add a creation rule for a machine's secrets.

        The pgp_keys and age_keys should be anchor references (e.g., "*admin_binarin_gpg").
        Returns True if the rule was added, False if it already exists.
        """
        rules = self.get_creation_rules()

        # Check if rule already exists
        if self.find_creation_rule(path_regex) is not None:
            return False

        # Build the rule structure
        rule = CommentedMap()
        rule["path_regex"] = path_regex

        key_groups = CommentedSeq()
        key_group = CommentedMap()

        # Add PGP keys
        if pgp_keys:
            pgp_seq = CommentedSeq()
            for key_ref in pgp_keys:
                pgp_seq.append(self._make_alias(key_ref))
            key_group["pgp"] = pgp_seq

        # Add age keys
        if age_keys:
            age_seq = CommentedSeq()
            for key_ref in age_keys:
                age_seq.append(self._make_alias(key_ref))
            key_group["age"] = age_seq

        key_groups.append(key_group)
        rule["key_groups"] = key_groups

        rules.append(rule)
        return True

    def _make_alias(self, anchor_name: str):
        """Create a YAML alias reference."""
        # Find the original key with this anchor
        keys = self.get_keys_section()
        for key in keys:
            if hasattr(key, "anchor") and key.anchor.value == anchor_name:
                # Create an alias that points to this key
                from ruamel.yaml.comments import CommentedBase

                class AliasPlaceholder(CommentedBase):
                    """A placeholder that will be serialized as an alias."""

                    yaml_tag = "tag:yaml.org,2002:str"

                    def __init__(self, anchor_name: str, original):
                        self._anchor_name = anchor_name
                        self._original = original

                # ruamel.yaml handles aliases automatically when the same
                # object is referenced multiple times. We return the original
                # key object so it creates an alias.
                return key

        # If anchor not found, just return the name (shouldn't happen)
        return anchor_name

    def update_machine_keys(
        self,
        machine: str,
        server_age_key: str,
        user_age_key: Optional[str] = None,
        admin_key: str = "admin_demandred_binarin",
    ) -> tuple[bool, bool]:
        """Update .sops.yaml with keys and rules for a machine.

        Returns (keys_added, rules_added) tuple.
        """
        keys_added = False
        rules_added = False

        # Add server key anchor
        server_anchor = f"server_{machine}"
        if self.add_key_anchor(server_anchor, server_age_key):
            keys_added = True

        # Add user key anchor if provided
        user_anchor = f"user_{machine}_binarin"
        if user_age_key and self.add_key_anchor(user_anchor, user_age_key):
            keys_added = True

        # Add creation rule for secrets.yaml
        secrets_regex = f"secrets/{machine}/secrets.yaml"
        age_refs = [admin_key, server_anchor]
        if self.add_creation_rule(
            secrets_regex,
            pgp_keys=["admin_binarin_gpg"],
            age_keys=age_refs,
        ):
            rules_added = True

        # Add creation rule for user-binarin.yaml
        user_regex = f"secrets/{machine}/user-binarin.yaml"
        user_age_refs = [admin_key]
        if user_age_key:
            user_age_refs.append(user_anchor)
        if self.add_creation_rule(
            user_regex,
            pgp_keys=["admin_binarin_gpg"],
            age_keys=user_age_refs,
        ):
            rules_added = True

        return keys_added, rules_added

    def has_machine_keys(self, machine: str) -> bool:
        """Check if a machine's keys are configured in .sops.yaml."""
        server_anchor = f"server_{machine}"
        return self.find_key_anchor(server_anchor) is not None
