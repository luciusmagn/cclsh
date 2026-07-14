PREFIX ?= /usr/local
BINDIR ?= $(PREFIX)/bin
DESTDIR ?=
SHELLS_FILE ?= /etc/shells
LOGIN_USER ?=
CCL ?= ccl
CCL_IMAGE ?=
CCL_SOURCE ?= ../ccl

.PHONY: build login-build ccl-kernel check integration-check install install-login-shell

build:
	CCLSH_CCL="$(CCL)" CCLSH_CCL_IMAGE="$(CCL_IMAGE)" scripts/build

login-build: CCL = $(CCL_SOURCE)/lx86cl64
login-build:
	CCLSH_CCL_IMAGE="$(CCL_IMAGE)" scripts/login-build \
		"$(CCL_SOURCE)" "$(CCL)" cclsh.attestation

ccl-kernel:
	scripts/ccl-kernel "$(CCL_SOURCE)"

check:
	CCLSH_CCL="$(CCL)" CCLSH_CCL_IMAGE="$(CCL_IMAGE)" scripts/check

integration-check:
	@set -e; \
	verify_current() { \
		kernel=$$(realpath -e cclsh); \
		scripts/verify-attestation \
			"$$kernel" "$$kernel.image" cclsh.attestation; \
	}; \
	if test -f cclsh.attestation; then \
		attested=1; \
		verify_current; \
	else \
		attested=0; \
		$(MAKE) build; \
	fi; \
	CCLSH_CCL="$(CCL)" CCLSH_CCL_IMAGE="$(CCL_IMAGE)" \
		scripts/integration-check; \
	if test "$$attested" -eq 1; then verify_current; fi

install:
	@if test -z "$(DESTDIR)" && test "$$(id -u)" -eq 0; then \
		echo "owner-only root install refused; use install-login-shell" >&2; \
		exit 2; \
	fi
	CCLSH_SKIP_BUILD=1 CCLSH_INSTALL_DIRECTORY="$(DESTDIR)$(BINDIR)" \
		scripts/install

install-login-shell:
	@if test -n "$(DESTDIR)"; then \
		echo "install-login-shell does not support DESTDIR" >&2; \
		exit 2; \
	fi
	@if test -z "$(LOGIN_USER)"; then \
		echo "install-login-shell requires LOGIN_USER" >&2; \
		exit 2; \
	fi
	CCLSH_SKIP_BUILD=1 \
	CCLSH_INSTALL_DIRECTORY="$(BINDIR)" \
	CCLSH_LOGIN_USER="$(LOGIN_USER)" \
	CCLSH_SHELLS_FILE="$(SHELLS_FILE)" \
	CCLSH_BUILD_ATTESTATION="$(CURDIR)/cclsh.attestation" \
		scripts/install
