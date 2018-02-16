#!/usr/bin/env bash

##############################################################################
# yubikey-ssh-setup.sh
# -----------
# Generates a new ssh key for a yubikey smartcard using GPG.
#
# Usage:
#       yubikey-ssh-setup
#
# :authors: Guillaume J. Charmes, @creack
# :date: February 16, 2018
# :version: 0.0.2
#
# Originally:
# :authors: Jess Frazelle, @jessfraz
# :date: 14 February 2018
# :version: 0.0.1
##############################################################################
set -e
set -o pipefail

function echo2() {
    echo $@ >&2
}

function usage() {
    echo2 "Usage:"
    echo2 "$0"
    echo2
    echo2 "Options:"
    echo2 "KEY=default. If no default, will be prompted interactively."
    echo2 "SUBKEY_LENGTH=4096"
    echo2 "SUBKEY_EXPIRE=0"
    echo2 "KEYID=$GPGKEY"
    echo2 "KEYBASENAME"
    echo2 "PUBLIC_KEY_URL=https://keybase.io/<keybasename>/key.asc"
    echo2 "SURNAME"
    echo2 "GIVENNAME"
    echo2 "EMAIL"
    echo2 "SEX"
    echo2
    echo2 "Example:"
    echo2 "EMAIL=guillaume@charmes.net GIVENNAME=Guillaume SURNAME=Charmes SEX=M KEYBASENAME=creack $0"
}

if [ "$1" = "-h" ] || [ "$1" = "--help" ]; then
    usage
    exit 1
fi

SUBKEY_LENGTH=${SUBKEY_LENGTH:=4096}
SUBKEY_EXPIRE=${SUBKEY_EXPIRE:=0}
KEYID=${KEYID:=$GPGKEY}

GPG=gpg2

while [ -z "$KEYID" ]; do
    echo2 "GPG Key ID:"
    read KEYID
done

while [ -z "$EMAIL" ]; do
    echo2 "Email:"
    read EMAIL
done

if [ -z "$GIVENNAME" ]; then
    echo2 "Given name:"
    read GIVENNAME
fi

if [ -z "$SURNAME" ]; then
    echo2 "Surname:"
    read SURNAME
fi

if [ -z "$SEX" ]; then
    echo2 "Sex:"
    read SEX
fi

if [ -z "$KEYBASENAME" ] && [ ! "$KEYBASENAME" = "skip" ]; then
    echo2 "Keybase name:"
    read KEYBASENAME
    if [ -z "$PUBLIC_KEY_URL" ] && [ ! -z "$KEYBASENAME" ]; then
	PUBLIC_KEY_URL="https://keybase.io/$KEYBASENAME/key.asc"
    fi
fi

if [ -z "$PUBLIC_KEY_URL" ]; then
    echo2 "Public key url:"
    read PUBLIC_KEY_URL
fi

function check_deps() {
    if ! hash pidof 2> /dev/null; then
	echo2 "Missing 'pidof'."
	exit 1
    fi
    if ! hash $GPG 2> /dev/null; then
	echo2 "Missing gpg command '$GPG'."
	exit 1
    fi
    if ! hash gpg-connect-agent 2> /dev/null; then
	echo2 "Missing 'gpg-connect-agent"
	exit 1
    fi
    if ! hash gsutil 2> /dev/null; then
	echo2 "Missing 'gsutil"
	exit 1
    fi
    if ! hash ssh-add 2> /dev/null; then
	echo2 "Missing 'ssh-add"
	exit 1
    fi
}

function restart_agent() {
	# Restart the gpg agent.
	# shellcheck disable=SC2046
	kill -9 $(pidof scdaemon) >/dev/null 2>&1
	# shellcheck disable=SC2046
	kill -9 $(pidof gpg-agent) >/dev/null 2>&1
	gpg-connect-agent /bye >/dev/null 2>&1
	gpg-connect-agent updatestartuptty /bye >/dev/null 2>&1
}

function setup_gnupghome() {
	# Create a temporary directory for GNUPGHOME.
	GNUPGHOME=$(mktemp -d)
	export GNUPGHOME
	echo2 "Temporary GNUPGHOME is $GNUPGHOME"
	echo2 "KeyID is ${KEYID}"

	# Create the gpg config file.
	echo2 "Setting up gpg.conf..."
	cat <<-EOF > "${GNUPGHOME}/gpg.conf"
	use-agent
	personal-cipher-preferences AES256 AES192 AES CAST5
	personal-digest-preferences SHA512 SHA384 SHA256 SHA224
	default-preference-list SHA512 SHA384 SHA256 SHA224 AES256 AES192 AES CAST5 ZLIB BZIP2 ZIP Uncompressed
	cert-digest-algo SHA512
	s2k-digest-algo SHA512
	s2k-cipher-algo AES256
	charset utf-8
	fixed-list-mode
	no-comments
	no-emit-version
	keyid-format 0xlong
	list-options show-uid-validity
	verify-options show-uid-validity
	with-fingerprint
	EOF

	echo2 "Setting up gpg-agent.conf..."
	cat <<-EOF > "${GNUPGHOME}/gpg-agent.conf"
	pinentry-program /usr/bin/pinentry
	enable-ssh-support
	use-standard-socket
	default-cache-ttl 600
	max-cache-ttl 7200
	EOF

	# Copy master key info to GNUPGHOME.
	# IMPORTANT: This is here as a stopper for anyone who runs this without reading it,
	# because it will exit here.
	# Modify this line to copy from a USB stick or your $HOME directory.
	echo2 "Copying master key from Google Cloud Storage..."
	gsutil -m cp gs://misc.j3ss.co/dotfiles/.gnupg/* "${GNUPGHOME}/"

	# Re-import the secret keys.
	$GPG --import "${GNUPGHOME}/secring.gpg"

	# Update the default key in the gpg config file.
	echo "default-key ${KEYID}" >> "${GNUPGHOME}/gpg.conf"

	restart_agent
}

function validate() {
	if $GPG --with-colons --list-key "$KEYID" | grep -q "No public key"; then
		echo "I don't know any key called $KEYID"
		exit 1
	fi
}

function generate_subkeys() {
	echo2 "Printing local secret keys..."
	$GPG --list-secret-keys

	echo2 "Generating subkeys..."

	echo2 "Generating signing subkey..."
	echo addkey$'\n'4$'\n'$SUBKEY_LENGTH$'\n'"$SUBKEY_EXPIRE"$'\n'save$'\n' | \
		$GPG --expert --batch --display-charset utf-8 \
		--command-fd 0 --edit-key "$KEYID"

	echo2 "Generating encryption subkey..."
	echo addkey$'\n'6$'\n'$SUBKEY_LENGTH$'\n'"$SUBKEY_EXPIRE"$'\n'save$'\n' | \
		$GPG --expert --batch --display-charset utf-8 \
		--command-fd 0 --edit-key "$KEYID"

	echo2 "Generating authentication subkey..."
	echo addkey$'\n'8$'\n'S$'\n'E$'\n'A$'\n'q$'\n'$SUBKEY_LENGTH$'\n'"$SUBKEY_EXPIRE"$'\n'save$'\n' | \
		$GPG --expert --batch --display-charset utf-8 \
		--command-fd 0 --edit-key "$KEYID"

	echo2 "Printing local secret keys..."
	$GPG --list-secret-keys
}

function move_keys_to_card() {
	echo2 "Moving signing subkey to card..."
	echo "key 2"$'\n'keytocard$'\n'1$'\n'y$'\n'save$'\n' | \
		$GPG --expert --batch --display-charset utf-8 \
		--command-fd 0 --edit-key "$KEYID"

	echo "key 3"$'\n'keytocard$'\n'2$'\n'y$'\n'save$'\n' | \
		$GPG --expert --batch --display-charset utf-8 \
		--command-fd 0 --edit-key "$KEYID"

	echo "key 4"$'\n'keytocard$'\n'3$'\n'y$'\n'save$'\n' | \
		$GPG --expert --batch --display-charset utf-8 \
		--command-fd 0 --edit-key "$KEYID"

	echo2 "Printing card status..."
	$GPG --card-status
}

function update_cardinfo() {
	# Edit the smart card name and info values.
	echo2 "Updating card holder name..."
	echo admin$'\n'name$'\n'$SURNAME$'\n'$GIVENNAME$'\n'quit$'\n' | \
		$GPG --expert --batch --display-charset utf-8 \
		--command-fd 0 --card-edit

	echo2 "Updating card language..."
	echo admin$'\n'lang$'\n'en$'\n'quit$'\n' | \
		$GPG --expert --batch --display-charset utf-8 \
		--command-fd 0 --card-edit

	echo2 "Updating card login..."
	echo admin$'\n'login$'\n'"$EMAIL"$'\n'quit$'\n' | \
		$GPG --expert --batch --display-charset utf-8 \
		--command-fd 0 --card-edit

	echo2 "Updating card public key url..."
	echo admin$'\n'url$'\n'$PUBLIC_KEY_URL$'\n'quit$'\n' | \
		$GPG --expert --batch --display-charset utf-8 \
		--command-fd 0 --card-edit

	echo2 "Updating card sex..."
	echo admin$'\n'sex$'\n'$SEX$'\n'quit$'\n' | \
		$GPG --expert --batch --display-charset utf-8 \
		--command-fd 0 --card-edit
}

function finalize() {
	echo2 "Printing card status..."
	$GPG --card-status

	echo2
	echo2 "Printing local secret keys..."
	$GPG --list-secret-keys

	echo2
	echo2 "Temporary GNUPGHOME is $GNUPGHOME"

	$GPG --armor --export "$KEYID" > "${HOME}/pubkey.txt"

	echo2
	echo2 "Public key is ${HOME}/pubkey.txt"
	echo2 "You should upload it to a public key server"

	echo2
	echo2 "Printing ssh key..."
	restart_agent
	ssh-add -L
}

check_deps
setup_gnupghome
validate
generate_subkeys
move_keys_to_card
update_cardinfo
finalize
