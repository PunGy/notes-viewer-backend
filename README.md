# notes-viewer-backend

## Prerequisite

- stack: >=3.7.1

### Additionally

OpenSSL: required by `http-client-tls`

Debian/Ubuntu:

```sh
sudo apt-get install libssl-dev zlib1g-dev
```

Fedora/RedHat:

```sh
sudo dnf install openssl-devel zlib-devel
```

## Running and Building

### Setup .env file

Copy `.env.example` to `.env` and adjust settings.

### Build

```bash
# Download dependencies and build
stack install

# Run
stack run
```
