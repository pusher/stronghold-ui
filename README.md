# DEPRECATED
`stronghold-ui` is officially deprecated and unmaintained.
It is recommended that users transition to alternatives.

# stronghold-ui
A UI for stronghold.

## Building:

```sh
stack build
```

## Running:

```sh
stack exec stronghold-ui -- path/to/config_file.conf
```

## Example config file:

```ini
stronghold-url = "http://localhost:5040"
github-client-id = "xxxxxxxxxxxxxxxxxxxx"
github-client-secret = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
authorised-users = ["YourGitHubUsername"]
port = 8000
session-secret-path = "./config/session_secret"
assets-path = "./assets"
```

Recommend putting this file under `./config`, as it is in the gitignore list

You can get a client id and secret by creating a GitHub application using the
following settings:

```
homepage: http://localhost:8000/
callback url: http://localhost:8000/auth/github/callback
```
