# prepare

    sudo apt install ldap-utils slapd

# new password:

    slappasswd -h {SSHA}

# Current user passwords:
test1: test
test2: test

# Start

    docker compose pull
    docker compose up
