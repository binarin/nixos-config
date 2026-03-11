# Brother Scanner/MFC Configuration Tool

This ExecPlan is a living document. The sections `Progress`, `Surprises & Discoveries`, `Decision Log`, and `Outcomes & Retrospective` must be kept up to date as work proceeds.

This document must be maintained in accordance with `../PLANS.md`.

## Purpose / Big Picture

Create a Python CLI tool (`brother-config`) to manage Brother ADS-2800W scanner/MFC settings programmatically. This enables declarative configuration management of:

- Address book entries and email groups
- SMTP settings for sending scans via email
- Scan to E-mail Server parameters
- SFTP profiles including server public key management

After implementation, a user can run commands like `brother-config address-book sync addresses.yaml` to declaratively configure the scanner from version-controlled YAML files, or `brother-config sftp-profile set 1 --host 192.168.1.10 --user scanner` to modify individual settings. This eliminates manual web UI configuration and enables infrastructure-as-code for scanner settings.

## Progress

- [x] (2026-03-11 20:25Z) Initial investigation of Brother web UI structure
- [x] (2026-03-11 20:35Z) Documented Address Book pages and XML export format
- [x] (2026-03-11 20:38Z) Documented SMTP settings page structure
- [x] (2026-03-11 20:40Z) Documented Scan to E-mail Server settings
- [x] (2026-03-11 20:42Z) Documented SFTP profile type selection and profile configuration
- [x] (2026-03-11 20:45Z) Documented public server key and client key pair management
- [x] (2026-03-11 20:48Z) Captured form field names and CSRF token handling
- [ ] Create Python project structure with pyproject.toml
- [ ] Implement HTTP session management with authentication
- [ ] Implement Address Book CRUD operations
- [ ] Implement SMTP settings management
- [ ] Implement Scan to E-mail Server settings management
- [ ] Implement SFTP profile management
- [ ] Implement public server key import/management
- [ ] Add CLI interface using Click or Typer
- [ ] Add YAML configuration file support for declarative management
- [ ] Test against live scanner

## Surprises & Discoveries

- Observation: Form field names are obfuscated hex identifiers (e.g., `Bec4` for profile name, `B1261` for email address)
  Evidence: JavaScript evaluation of form elements shows names like `Bec4`, `Bedd`, `Bef6` rather than semantic names

- Observation: All forms require CSRF tokens for submission
  Evidence: Hidden `CSRFToken` field present in all forms, value changes per session

- Observation: Address book uses vCard 4.0 XML format with Brother-specific namespace extensions
  Evidence: Exported files use `xmlns="urn:ietf:params:xml:ns:vcard-4.0"` with `xmlns:ba="http://schemas.brother.info/mfc/controller/phx/2013/04/addressbookschemakeywords"`

- Observation: No explicit "test connection" buttons found in web UI for SMTP or SFTP
  Evidence: Searched all relevant pages; only Submit/Cancel buttons present

- Observation: Groups reference addresses using a specific ID format
  Evidence: Group XML uses `ba:contact-id="speed.standard.1.1"` to reference address #001 email #1

## Decision Log

- Decision: Use requests library with session management for HTTP operations
  Rationale: Need to maintain login session and CSRF tokens across multiple requests; requests.Session handles cookies automatically
  Date/Author: 2026-03-11 / Investigation phase

- Decision: Implement XML import/export as primary mechanism for address book sync
  Rationale: Brother provides clean XML export/import that handles all complexity; form-based editing requires tracking obfuscated field names per page
  Date/Author: 2026-03-11 / Investigation phase

- Decision: For SFTP profiles, use form submission since no XML export exists
  Rationale: Profile configuration only available via web forms; need to parse form field names dynamically
  Date/Author: 2026-03-11 / Investigation phase

## Outcomes & Retrospective

Investigation phase completed. Web UI structure is well understood. Main challenge will be handling obfuscated form field names which may vary between firmware versions.

## Context and Orientation

The Brother ADS-2800W is a network document scanner with a web-based administration interface. The scanner IP is `192.168.2.58` and requires password authentication (stored in sops secrets for production use).

### Web UI Structure

The web interface is organized into these main sections:

    http://192.168.2.58/
    ├── /general/              - General status and settings
    ├── /address/              - Address book management
    │   ├── speeddial.html     - Individual addresses (paginated, 20 per page)
    │   ├── groupdial.html     - Group configuration
    │   ├── import.html        - Import addresses from XML
    │   └── export.html        - Export addresses to XML
    ├── /fax/ifax/             - Email settings
    │   ├── ifax.html          - Email overview
    │   └── send.html          - Email send settings
    ├── /scan/                 - Scan settings
    │   ├── scantoemail.html   - Scan to Email Server parameters
    │   ├── netscan.html       - Profile type selection (FTP/SFTP/Network/SharePoint)
    │   ├── scanprofile.html   - Profile list
    │   └── profile_sftp.html  - Individual SFTP profile settings
    ├── /net/                  - Network settings
    │   └── net/email.html     - SMTP configuration
    └── /net/security/certificate/
        ├── keypair.html       - Client SSH key pairs
        ├── pubkey.html        - Server public keys
        └── import_pubkey.html - Import server public key

### Authentication

Login is performed by POST to the main page with password field. The session is cookie-based and must be maintained across requests.

### Address Book XML Format

The scanner exports/imports address books using vCard 4.0 with Brother extensions.

**address.xml** - Individual email addresses:

    <?xml version="1.0" encoding="UTF-8"?>
    <vcards xmlns="urn:ietf:params:xml:ns:vcard-4.0"
            xmlns:ba="http://schemas.brother.info/mfc/controller/phx/2013/04/addressbookschemakeywords"
            ba:model="ADS-2800W"
            ba:dialkind="Speed">
      <vcard ba:dial-id="1">
        <fn><text>CONTACT NAME</text></fn>
        <email ba:index="1"><text>email@example.com</text></email>
      </vcard>
    </vcards>

**group.xml** - Email groups:

    <?xml version="1.0" encoding="UTF-8"?>
    <vcards xmlns="urn:ietf:params:xml:ns:vcard-4.0"
            xmlns:ba="http://schemas.brother.info/mfc/controller/phx/2013/04/addressbookschemakeywords"
            ba:model="ADS-2800W"
            ba:dialkind="Group">
      <vcard ba:group-id="1" ba:address="speed.standard.3">
        <fn><text>GROUP NAME</text></fn>
        <ba:members>
          <ba:member ba:contact-id="speed.standard.1.1"></ba:member>
          <ba:member ba:contact-id="speed.standard.2.1"></ba:member>
        </ba:members>
      </vcard>
    </vcards>

The `ba:contact-id` format is `speed.standard.{address_slot}.{email_index}` where address_slot is 1-based and email_index is typically 1.

### SFTP Profile Configuration

SFTP profiles are configured across two pages:

1. **Type selection** (`/scan/netscan.html`): Sets each of 25 profiles to FTP, SFTP, Network, or SharePoint
2. **Profile details** (`/scan/profile_sftp.html?val=N`): Configures individual profile

SFTP profile fields (mapped from obfuscated form names based on order/context):
- Profile name
- Host address
- Username
- Authentication method (password or public key)
- Password (if password auth)
- Client Key Pair (dropdown, references keys from /net/security/certificate/keypair.html)
- Public server key (dropdown, references keys from /net/security/certificate/pubkey.html)
- Save directory
- Port number
- Scan parameters (quality, file type, document size, margins, etc.)

### SMTP Settings

SMTP configuration at `/net/net/email.html`:
- Server address
- Port
- Authentication method (None or SMTP-AUTH)
- Account name and password
- SSL/TLS setting (None, SSL, or TLS)
- Verify server certificate checkbox
- Device email address

### Public Key Management

Server public keys (for SFTP host verification) are managed at:
- List: `/net/security/certificate/pubkey.html`
- Import: `/net/security/certificate/import_pubkey.html` (file upload)
- Delete: `/net/security/certificate/delete_pubkey.html?idx=N`

Client key pairs (scanner's SSH key) are managed at:
- List: `/net/security/certificate/keypair.html`
- Create: `/net/security/certificate/create_keypair.html`
- Export public key: `/net/security/certificate/export_pubkey.html?idx=N`
- Delete: `/net/security/certificate/delete_keypair.html?idx=N`

## Plan of Work

### Milestone 1: Project Setup and HTTP Session Management

Create the Python project structure and implement authenticated HTTP session management.

Create `tools/brother-config/` directory with:
- `pyproject.toml` - Project configuration with dependencies (requests, click, pyyaml, lxml)
- `brother_config/__init__.py` - Package init
- `brother_config/session.py` - HTTP session with login/logout and CSRF handling
- `brother_config/cli.py` - CLI entry point

The session manager must:
1. Login by POSTing password to root URL
2. Extract and maintain CSRF tokens from page responses
3. Include CSRF token in all form submissions
4. Handle session expiry gracefully

### Milestone 2: Address Book Management

Implement address book operations using XML import/export.

Create `brother_config/address_book.py` with:
- `export_addresses()` - Download current address.xml and group.xml
- `import_addresses()` - Upload address.xml and group.xml
- `parse_addresses()` - Parse XML to Python data structures
- `generate_addresses()` - Generate XML from Python data structures
- `sync_addresses()` - Compare and sync from YAML configuration

CLI commands:
- `brother-config address-book export [--output-dir DIR]`
- `brother-config address-book import [--address-file FILE] [--group-file FILE]`
- `brother-config address-book sync CONFIG_FILE`

### Milestone 3: SMTP Settings Management

Implement SMTP configuration.

Create `brother_config/smtp.py` with:
- `get_smtp_settings()` - Parse current SMTP config from web page
- `set_smtp_settings()` - Submit SMTP configuration form
- `sync_smtp()` - Compare and sync from YAML configuration

CLI commands:
- `brother-config smtp get`
- `brother-config smtp set --server HOST --port PORT --auth-method METHOD ...`
- `brother-config smtp sync CONFIG_FILE`

### Milestone 4: Scan to E-mail Server Settings

Implement scan-to-email parameters.

Create `brother_config/scan_email.py` with similar get/set/sync pattern.

### Milestone 5: SFTP Profile Management

Implement SFTP profile configuration including key management.

Create `brother_config/sftp.py` with:
- `list_profiles()` - Get all profile names and types
- `get_profile()` - Get specific profile configuration
- `set_profile()` - Configure a profile
- `set_profile_type()` - Change profile type (FTP/SFTP/Network/SharePoint)

Create `brother_config/keys.py` with:
- `list_server_keys()` - List imported server public keys
- `import_server_key()` - Upload a server public key file
- `delete_server_key()` - Remove a server public key
- `list_client_keypairs()` - List scanner's SSH keypairs
- `create_client_keypair()` - Generate new keypair on scanner
- `export_client_public_key()` - Download scanner's public key
- `delete_client_keypair()` - Remove a keypair

CLI commands:
- `brother-config sftp list`
- `brother-config sftp get PROFILE_NUMBER`
- `brother-config sftp set PROFILE_NUMBER [OPTIONS]`
- `brother-config keys server-keys list`
- `brother-config keys server-keys import FILE [--name NAME]`
- `brother-config keys server-keys delete NAME`
- `brother-config keys client-keys list`
- `brother-config keys client-keys export NAME [--output FILE]`

### Milestone 6: Declarative Configuration Support

Add YAML configuration file support for full declarative management.

Create `brother_config/declarative.py` with:
- `load_config()` - Load YAML configuration
- `sync_all()` - Apply full configuration to scanner

Example configuration file format:

    scanner:
      host: 192.168.2.58
      password: !sops-secret brother/web-password

    smtp:
      server: smtp.gmail.com
      port: 587
      auth: smtp-auth
      username: scanner@example.com
      password: !sops-secret brother/smtp-password
      tls: true
      device_email: scanner@example.com

    address_book:
      addresses:
        - slot: 1
          name: Alice
          email: alice@example.com
        - slot: 2
          name: Bob
          email: bob@example.com
      groups:
        - slot: 3
          name: Team
          group_id: 1
          members: [1, 2]

    sftp_profiles:
      - slot: 1
        name: paperless
        type: sftp
        host: 192.168.1.10
        username: scanner
        auth: public_key
        client_key: brother
        server_key: paperless_host_key
        directory: /incoming
        port: 22

## Concrete Steps

Working directory: `/home/binarin/personal-workspace/nixos-config`

### Step 1: Create project structure

    mkdir -p tools/brother-config/brother_config
    touch tools/brother-config/brother_config/__init__.py

### Step 2: Create pyproject.toml

Create `tools/brother-config/pyproject.toml` with dependencies on requests, click, pyyaml, lxml.

### Step 3: Implement session management

Create `tools/brother-config/brother_config/session.py` implementing the BrotherSession class.

Test by running:

    cd tools/brother-config
    nix develop
    python -c "from brother_config.session import BrotherSession; s = BrotherSession('192.168.2.58'); s.login('password'); print(s.get('/general/status.html').status_code)"

Expected: `200`

### Step 4: Implement address book operations

Create `tools/brother-config/brother_config/address_book.py`.

Test by running:

    brother-config --host 192.168.2.58 address-book export --output-dir /tmp/brother-export
    ls /tmp/brother-export/

Expected: `address.xml  group.xml`

Continue with remaining milestones following the same pattern.

## Validation and Acceptance

### Address Book
1. Export current address book: `brother-config address-book export`
2. Modify exported YAML/XML
3. Re-import: `brother-config address-book import`
4. Verify via web UI that changes are reflected

### SMTP Settings
1. Get current settings: `brother-config smtp get`
2. Verify output matches web UI
3. Modify setting: `brother-config smtp set --port 465`
4. Verify via web UI

### SFTP Profiles
1. List profiles: `brother-config sftp list`
2. Verify matches web UI profile list
3. Import a server key: `brother-config keys server-keys import /path/to/key.pub`
4. Verify key appears in web UI dropdown
5. Configure profile to use new key
6. Perform test scan (manual, via scanner hardware)

## Idempotence and Recovery

All operations are idempotent:
- Setting values to their current values is a no-op
- Address book sync only modifies changed entries
- Failed operations can be retried safely

The scanner maintains its own persistent storage; tool operations only affect configuration, not scanned documents or logs.

## Artifacts and Notes

### Downloaded XML samples

**address.xml**:

    <?xml version="1.0" encoding="UTF-8"?>
    <vcards xmlns="urn:ietf:params:xml:ns:vcard-4.0"
            xmlns:ba="http://schemas.brother.info/mfc/controller/phx/2013/04/addressbookschemakeywords"
            ba:model="ADS-2800W" ba:dialkind="Speed">
      <vcard ba:dial-id="1">
        <fn><text>ALEXEY</text></fn>
        <email ba:index="1"><text>binarin@binarin.info</text></email>
      </vcard>
      <vcard ba:dial-id="2">
        <fn><text>MARINA</text></fn>
        <email ba:index="1"><text>lunamk8@gmail.com</text></email>
      </vcard>
    </vcards>

**group.xml**:

    <?xml version="1.0" encoding="UTF-8"?>
    <vcards xmlns="urn:ietf:params:xml:ns:vcard-4.0"
            xmlns:ba="http://schemas.brother.info/mfc/controller/phx/2013/04/addressbookschemakeywords"
            ba:model="ADS-2800W" ba:dialkind="Group">
      <vcard ba:group-id="1" ba:address="speed.standard.3">
        <fn><text>US</text></fn>
        <ba:members>
          <ba:member ba:contact-id="speed.standard.1.1"></ba:member>
          <ba:member ba:contact-id="speed.standard.2.1"></ba:member>
        </ba:members>
      </vcard>
    </vcards>

### Form field name samples (may vary by firmware)

SFTP Profile form (`/scan/profile_sftp.html`):
- `Bec4` - Profile name
- `Bedd` - Host address
- `Bef6` - Username
- `Bf0f` - Auth method (radio)
- `Bf28` - Password
- `Bf5a` - Client key pair (select)
- `Bf73` - Public server key (select)
- `Bf8c` - Directory
- `B1022` - Port number

### Key URLs

- Login: POST to `/` with password field
- Address export: GET `/address/address.xml` and `/address/group.xml`
- Address import: POST multipart to `/address/import.html`
- SMTP settings: `/net/net/email.html`
- SFTP profile types: `/scan/netscan.html`
- SFTP profile config: `/scan/profile_sftp.html?val=N&pageid=M`
- Server public keys: `/net/security/certificate/pubkey.html`
- Import server key: POST multipart to `/net/security/certificate/import_pubkey.html`

## Interfaces and Dependencies

### Python Dependencies

- `requests` - HTTP client with session support
- `click` - CLI framework
- `pyyaml` - YAML parsing for configuration files
- `lxml` - XML parsing for address book

### Key Interfaces

In `brother_config/session.py`:

    class BrotherSession:
        def __init__(self, host: str, password: str | None = None):
            """Initialize session with scanner host."""

        def login(self, password: str) -> None:
            """Authenticate with scanner. Raises AuthError on failure."""

        def get(self, path: str) -> requests.Response:
            """GET request with session cookies."""

        def post(self, path: str, data: dict, files: dict | None = None) -> requests.Response:
            """POST request with CSRF token and session cookies."""

        def get_csrf_token(self, page_content: str) -> str:
            """Extract CSRF token from page HTML."""

In `brother_config/address_book.py`:

    @dataclass
    class Address:
        slot: int
        name: str
        email: str

    @dataclass
    class Group:
        slot: int
        group_id: int
        name: str
        member_slots: list[int]

    def export_addresses(session: BrotherSession) -> tuple[list[Address], list[Group]]:
        """Download and parse current address book."""

    def import_addresses(session: BrotherSession,
                         addresses: list[Address],
                         groups: list[Group]) -> None:
        """Upload address book configuration."""

In `brother_config/sftp.py`:

    @dataclass
    class SFTPProfile:
        slot: int
        name: str
        host: str
        username: str
        auth_method: Literal["password", "public_key"]
        password: str | None
        client_keypair: str | None
        server_key: str | None
        directory: str
        port: int
        # ... scan parameters

    def get_profile(session: BrotherSession, slot: int) -> SFTPProfile:
        """Get SFTP profile configuration."""

    def set_profile(session: BrotherSession, profile: SFTPProfile) -> None:
        """Configure SFTP profile."""

---
*Plan created: 2026-03-11*
*Last updated: 2026-03-11 - Initial investigation complete*
