#!/bin/sh
# Regression checks for the privileged shell-registration boundary.
set -eu
cd "$(dirname "$0")/.."

if [ "$(id -u)" -eq 0 ]; then
    temporary_directory=$(mktemp -d /run/cclsh-install-check.XXXXXX)
else
    temporary_directory=$(
        mktemp -d "${TMPDIR:-/tmp}/cclsh-install-check.XXXXXX"
    )
fi
lock_holder=
login_temporary_directory=
cleanup()
{
    if [ -n "$lock_holder" ]; then
        kill "$lock_holder" 2>/dev/null || true
        wait "$lock_holder" 2>/dev/null || true
    fi
    rm -rf -- "$temporary_directory"
    if [ -n "$login_temporary_directory" ]; then
        rm -rf -- "$login_temporary_directory"
    fi
}
trap cleanup 0
trap 'exit 129' 1
trap 'exit 130' 2
trap 'exit 143' 15

source_shell=$temporary_directory/source-cclsh
source_image=$temporary_directory/source-cclsh.image
install_directory=$temporary_directory/bin
shell_path=$install_directory/cclsh
shells_file=$temporary_directory/shells
attestation=$temporary_directory/cclsh.attestation

write_test_attestation()
{
    attested_kernel=$1
    attested_image=$2
    kernel_hash=$(scripts/file-sha256 "$attested_kernel")
    image_hash=$(scripts/file-sha256 "$attested_image")
    {
        printf '%s\n' 'cclsh-login-build-v1'
        printf 'kernel-sha256 %s\n' "$kernel_hash"
        printf 'image-sha256 %s\n' "$image_hash"
        for patch in patches/ccl-linux-xstate.patch patches/ccl-cclsh-argv.patch
        do
            patch_hash=$(scripts/file-sha256 "$patch")
            printf 'patch-sha256 %s %s\n' \
                "$(basename "$patch")" \
                "$patch_hash"
        done
    } >"$attestation"
}

cat >"$source_shell" <<'EOF'
#!/bin/sh
case "${1:-}" in
    --version)
        printf '%s\n' 'cclsh test'
        exit 0
        ;;
    -c)
        exit 0
        ;;
    *)
        exit 2
        ;;
esac
EOF
printf '%s\n' 'test image' >"$source_image"
printf '%s\n' '/bin/sh' >"$shells_file"
chmod 755 "$source_shell"
chmod 644 "$source_image"
chmod 640 "$shells_file"
install -d -m 700 "$install_directory"

{
    printf '%s\n' 'cclsh-login-build-v1'
    printf 'kernel-sha256 \n'
    printf 'image-sha256 \n'
    for patch in patches/ccl-linux-xstate.patch patches/ccl-cclsh-argv.patch
    do
        printf 'patch-sha256 %s %s\n' \
            "$(basename "$patch")" \
            "$(scripts/file-sha256 "$patch")"
    done
} >"$attestation"
if scripts/verify-attestation \
     "$temporary_directory/missing-kernel" \
     "$temporary_directory/missing-image" \
     "$attestation" >/dev/null 2>&1
then
    echo "attestation accepted missing artifacts with blank hashes" >&2
    exit 1
fi

bad_hash_bin=$temporary_directory/bad-hash-bin
mkdir "$bad_hash_bin"
printf '%s\n' '#!/bin/sh' 'echo not-a-sha256-digest' \
    >"$bad_hash_bin/sha256sum"
chmod 755 "$bad_hash_bin/sha256sum"
if PATH="$bad_hash_bin:$PATH" scripts/file-sha256 "$source_shell" \
     >/dev/null 2>&1
then
    echo "file hash accepted malformed sha256sum output" >&2
    exit 1
fi

write_test_attestation "$source_shell" "$source_image"
if [ "$(id -u)" -eq 0 ]; then
    TMPDIR=$temporary_directory/missing-attestation-tmp \
        scripts/verify-attestation "$source_shell" "$source_image" \
            "$attestation"
fi
printf '%s\n' mutation >>"$source_image"
if scripts/verify-attestation "$source_shell" "$source_image" \
     "$attestation" >/dev/null 2>&1
then
    echo "attestation accepted a changed image" >&2
    exit 1
fi
printf '%s\n' 'test image' >"$source_image"
write_test_attestation "$source_shell" "$source_image"

for timeout_setting in \
    CCLSH_PROBE_TIMEOUT=0 \
    CCLSH_PROBE_KILL_AFTER=0 \
    CCLSH_LOCK_TIMEOUT=invalid
do
    timeout_name=${timeout_setting%%=*}
    timeout_value=${timeout_setting#*=}
    if env "$timeout_name=$timeout_value" \
         CCLSH_SKIP_BUILD=1 \
         CCLSH_KERNEL_ARTIFACT="$source_shell" \
         CCLSH_IMAGE_ARTIFACT="$source_image" \
         CCLSH_INSTALL_DIRECTORY="$temporary_directory/timeout-bin" \
         scripts/install >/dev/null 2>&1
    then
        echo "install accepted invalid timeout: $timeout_setting" >&2
        exit 1
    fi
done

prebuilt_refusal=$temporary_directory/prebuilt-refusal
set +e
CCLSH_CCL=$temporary_directory/missing-ccl \
CCLSH_LOGIN_USER=unused-login-user \
CCLSH_INSTALL_DIRECTORY="$prebuilt_refusal" \
    scripts/install >$temporary_directory/prebuilt.stdout \
                    2>$temporary_directory/prebuilt.stderr
prebuilt_status=$?
set -e
if [ "$prebuilt_status" -ne 2 ] ||
   ! grep -Fqx \
       'cclsh install: login installation requires prebuilt attested artifacts; set CCLSH_SKIP_BUILD=1' \
       "$temporary_directory/prebuilt.stderr" ||
   [ -e "$prebuilt_refusal" ] || [ -L "$prebuilt_refusal" ]
then
    echo "login install did not refuse an implicit privileged build" >&2
    exit 1
fi

if [ "$(id -u)" -eq 0 ]; then
    if CCLSH_SKIP_BUILD=1 \
       CCLSH_KERNEL_ARTIFACT="$source_shell" \
       CCLSH_IMAGE_ARTIFACT="$source_image" \
       CCLSH_INSTALL_DIRECTORY="$temporary_directory/root-login-bin" \
       CCLSH_LOGIN_USER=root \
       CCLSH_SHELLS_FILE="$shells_file" \
       CCLSH_BUILD_ATTESTATION="$attestation" \
       scripts/install >/dev/null 2>&1
    then
        echo "login install accepted root as its probe user" >&2
        exit 1
    fi
    if [ -e "$temporary_directory/root-login-bin" ]; then
        echo "rejected root login install wrote its destination" >&2
        exit 1
    fi
fi

CCLSH_SKIP_BUILD=1 \
CCLSH_KERNEL_ARTIFACT="$source_shell" \
CCLSH_IMAGE_ARTIFACT="$source_image" \
CCLSH_INSTALL_DIRECTORY="$install_directory" \
scripts/install >/dev/null
if [ "$(stat -c %a "$install_directory")" != 700 ]; then
    echo "install changed the mode of an existing destination" >&2
    exit 1
fi
if [ ! -L "$shell_path" ] || [ ! -L "$shell_path.image" ]; then
    echo "install did not activate stable symlinks" >&2
    exit 1
fi
first_release=$(realpath "$shell_path")
if [ ! -f "$first_release.image" ]; then
    echo "install did not keep the release image beside its kernel" >&2
    exit 1
fi

printf '%s\n' '# second release' >>"$source_shell"
CCLSH_SKIP_BUILD=1 \
CCLSH_KERNEL_ARTIFACT="$source_shell" \
CCLSH_IMAGE_ARTIFACT="$source_image" \
CCLSH_INSTALL_DIRECTORY="$install_directory" \
scripts/install >/dev/null
second_release=$(realpath "$shell_path")
if [ "$first_release" = "$second_release" ] || [ ! -f "$first_release" ]; then
    echo "install did not atomically retain and switch releases" >&2
    exit 1
fi

stale_directory=$temporary_directory/stale-rollback-bin
stale_ready=$temporary_directory/stale-rollback-ready
stale_release=$temporary_directory/stale-rollback-release
mkdir "$stale_directory"
mkfifo "$stale_ready" "$stale_release"
cp "$source_shell" "$stale_directory/cclsh"
cp "$source_image" "$stale_directory/cclsh.image"
chmod 755 "$stale_directory/cclsh"
chmod 600 "$stale_directory/cclsh.image"
/usr/bin/timeout --kill-after=1 10 sh -c '
    printf "%s\n" "$$" >"$4"
    IFS= read -r release <"$5"
    exec env \
        CCLSH_SKIP_BUILD=1 \
        CCLSH_KERNEL_ARTIFACT="$2" \
        CCLSH_IMAGE_ARTIFACT="$3" \
        CCLSH_INSTALL_DIRECTORY="$1" \
        scripts/install
' sh "$stale_directory" "$source_shell" "$source_image" \
    "$stale_ready" "$stale_release" >/dev/null &
lock_holder=$!
IFS= read -r stale_pid <"$stale_ready"
stale_path=$stale_directory/.cclsh-rollback-kernel.$stale_pid
printf '%s\n' sentinel >"$stale_path"
printf '%s\n' release >"$stale_release"
set +e
wait "$lock_holder"
stale_status=$?
set -e
lock_holder=
if [ "$stale_status" -ne 0 ] || [ ! -f "$stale_path" ] ||
   [ "$(cat "$stale_path")" != sentinel ]
then
    echo "install overwrote a stale process-id recovery file" >&2
    exit 1
fi

cat >"$source_shell" <<'EOF'
#!/bin/sh
exit 1
EOF
if CCLSH_SKIP_BUILD=1 \
   CCLSH_KERNEL_ARTIFACT="$source_shell" \
   CCLSH_IMAGE_ARTIFACT="$source_image" \
   CCLSH_INSTALL_DIRECTORY="$install_directory" \
   scripts/install >/dev/null 2>&1
then
    echo "install accepted a failed clean probe" >&2
    exit 1
fi
if [ "$(realpath "$shell_path")" != "$second_release" ]; then
    echo "failed install changed the active release" >&2
    exit 1
fi

printf '%s\n' '#!/bin/sh' 'sleep 10' >"$source_shell"
chmod 755 "$source_shell"
set +e
/usr/bin/timeout --preserve-status --signal=TERM --kill-after=1 3 \
    env CCLSH_PROBE_TIMEOUT=0.2 CCLSH_PROBE_KILL_AFTER=0.2 \
        CCLSH_SKIP_BUILD=1 \
        CCLSH_KERNEL_ARTIFACT="$source_shell" \
        CCLSH_IMAGE_ARTIFACT="$source_image" \
        CCLSH_INSTALL_DIRECTORY="$install_directory" \
        scripts/install >/dev/null 2>&1
hanging_status=$?
set -e
if [ "$hanging_status" -eq 0 ] || [ "$hanging_status" -eq 143 ] ||
   [ "$hanging_status" -eq 137 ]
then
    echo "install did not bound a hanging candidate: $hanging_status" >&2
    exit 1
fi
if [ "$(realpath "$shell_path")" != "$second_release" ]; then
    echo "hanging candidate changed the active release" >&2
    exit 1
fi

if [ "$(id -u)" -eq 0 ] && id nobody >/dev/null 2>&1; then
    login_temporary_directory=$(mktemp -d /run/cclsh-install-check.XXXXXX)
    chmod 755 "$login_temporary_directory"
    login_install_directory=$login_temporary_directory/bin
    login_shell=$login_install_directory/cclsh
    install -d -m 755 "$login_install_directory"
    chmod 755 "$temporary_directory"
    chmod 755 "$install_directory"
    nobody_uid=$(id -u nobody)
    cat >"$source_shell" <<EOF
#!/bin/sh
if test "\$(id -u)" = "$nobody_uid"; then
    exit 1
fi
case "\${1:-}" in
    --version|-c) exit 0 ;;
    *) exit 2 ;;
esac
EOF
    write_test_attestation "$source_shell" "$source_image"
    scripts/verify-attestation "$source_shell" "$source_image" \
        "$attestation"
    CCLSH_SKIP_BUILD=1 \
    CCLSH_KERNEL_ARTIFACT="$source_shell" \
    CCLSH_IMAGE_ARTIFACT="$source_image" \
    CCLSH_INSTALL_DIRECTORY="$login_install_directory" \
    scripts/install >/dev/null
    unsafe_directory=$login_temporary_directory/unsafe-login-bin
    install -d -m 755 "$unsafe_directory"
    chown "$nobody_uid:$(id -g nobody)" "$unsafe_directory"
    if CCLSH_SKIP_BUILD=1 \
       CCLSH_KERNEL_ARTIFACT="$source_shell" \
       CCLSH_IMAGE_ARTIFACT="$source_image" \
       CCLSH_INSTALL_DIRECTORY="$unsafe_directory" \
       CCLSH_LOGIN_USER=nobody \
       CCLSH_SHELLS_FILE="$shells_file" \
       CCLSH_BUILD_ATTESTATION="$attestation" \
       scripts/install >/dev/null 2>&1
    then
        echo "login install accepted an untrusted destination" >&2
        exit 1
    fi
    if find "$unsafe_directory" -mindepth 1 -print -quit | grep -q .; then
        echo "rejected login destination was modified" >&2
        exit 1
    fi
    before_target_failure=$(realpath "$login_shell")
    if CCLSH_SKIP_BUILD=1 \
       CCLSH_KERNEL_ARTIFACT="$source_shell" \
       CCLSH_IMAGE_ARTIFACT="$source_image" \
       CCLSH_INSTALL_DIRECTORY="$login_install_directory" \
       CCLSH_LOGIN_USER=nobody \
       CCLSH_SHELLS_FILE="$shells_file" \
       CCLSH_BUILD_ATTESTATION="$attestation" \
       scripts/install >/dev/null 2>&1
    then
        echo "install accepted a failed target-user probe" >&2
        exit 1
    fi
    if [ "$(realpath "$login_shell")" != "$before_target_failure" ]; then
        echo "target-user probe failure changed the active release" >&2
        exit 1
    fi

    cat >"$source_shell" <<'EOF'
#!/bin/sh
case "${1:-}" in
    --version|-c) exit 0 ;;
    *) exit 2 ;;
esac
EOF
    write_test_attestation "$source_shell" "$source_image"
    scripts/verify-attestation "$source_shell" "$source_image" \
        "$attestation"
    before_registration_failure=$(realpath "$login_shell")
    if CCLSH_SKIP_BUILD=1 \
       CCLSH_KERNEL_ARTIFACT="$source_shell" \
       CCLSH_IMAGE_ARTIFACT="$source_image" \
       CCLSH_INSTALL_DIRECTORY="$login_install_directory" \
       CCLSH_LOGIN_USER=nobody \
       CCLSH_SHELLS_FILE="$temporary_directory/./shells" \
       CCLSH_BUILD_ATTESTATION="$attestation" \
       scripts/install >/dev/null 2>&1
    then
        echo "install accepted a failed shell registration" >&2
        exit 1
    fi
    if [ "$(realpath "$login_shell")" != "$before_registration_failure" ]; then
        echo "registration failure did not restore the active release" >&2
        exit 1
    fi

    rollback_directory=$login_temporary_directory/rollback-bin
    rollback_shell=$rollback_directory/cclsh
    rollback_image=$rollback_directory/cclsh.image
    rollback_shell_copy=$temporary_directory/rollback-cclsh.expected
    rollback_image_copy=$temporary_directory/rollback-image.expected
    rollback_shells=$temporary_directory/rollback-shells
    install -d -m 755 "$rollback_directory"
    printf '%s\n' '#!/bin/sh' 'exit 71' >"$rollback_shell"
    printf '%s\n' 'previous regular image' >"$rollback_image"
    chmod 755 "$rollback_shell"
    chmod 640 "$rollback_image"
    cp "$rollback_shell" "$rollback_shell_copy"
    cp "$rollback_image" "$rollback_image_copy"
    printf '%s\n' /bin/sh >"$rollback_shells"
    chmod 640 "$rollback_shells"
    if CCLSH_SKIP_BUILD=1 \
       CCLSH_KERNEL_ARTIFACT="$source_shell" \
       CCLSH_IMAGE_ARTIFACT="$source_image" \
       CCLSH_INSTALL_DIRECTORY="$rollback_directory" \
       CCLSH_LOGIN_USER=nobody \
       CCLSH_SHELLS_FILE="$temporary_directory/./rollback-shells" \
       CCLSH_BUILD_ATTESTATION="$attestation" \
       scripts/install >/dev/null 2>&1
    then
        echo "regular-file rollback test unexpectedly registered" >&2
        exit 1
    fi
    if [ -L "$rollback_shell" ] || [ -L "$rollback_image" ] ||
       ! cmp -s "$rollback_shell_copy" "$rollback_shell" ||
       ! cmp -s "$rollback_image_copy" "$rollback_image" ||
       find "$rollback_directory" -maxdepth 1 \
           -name '.cclsh-rollback-*' -print -quit | grep -q .
    then
        echo "registration failure did not restore regular files" >&2
        exit 1
    fi

    private_source=$temporary_directory/private-source-cclsh
    private_image=$temporary_directory/private-source-cclsh.image
    private_directory=$login_temporary_directory/private-bin
    private_shell=$private_directory/cclsh
    private_shells=$temporary_directory/private-shells
    private_failure=$temporary_directory/private-probe-failure
    cat >"$private_source" <<EOF
#!/bin/sh
if test "\$(id -u)" = "$nobody_uid" && test -e "$private_failure"; then
    exit 1
fi
case "\${1:-}" in
    --version|-c) exit 0 ;;
    *) exit 2 ;;
esac
EOF
    printf '%s\n' 'private image' >"$private_image"
    printf '%s\n' '/bin/sh' >"$private_shells"
    chmod 755 "$private_source"
    chmod 644 "$private_image"
    chmod 640 "$private_shells"
    install -d -m 755 "$private_directory"

    write_test_attestation "$private_source" "$private_image"
    printf '%s\n' tampered >>"$attestation"
    if CCLSH_SKIP_BUILD=1 \
       CCLSH_KERNEL_ARTIFACT="$private_source" \
       CCLSH_IMAGE_ARTIFACT="$private_image" \
       CCLSH_INSTALL_DIRECTORY="$private_directory" \
       CCLSH_LOGIN_USER=nobody \
       CCLSH_SHELLS_FILE="$private_shells" \
       CCLSH_BUILD_ATTESTATION="$attestation" \
       scripts/install >/dev/null 2>&1
    then
        echo "install accepted a stale build attestation" >&2
        exit 1
    fi
    if [ -e "$private_shell" ] || [ -L "$private_shell" ]; then
        echo "invalid attestation activated a shell" >&2
        exit 1
    fi

    write_test_attestation "$private_source" "$private_image"
    CCLSH_SKIP_BUILD=1 \
    CCLSH_KERNEL_ARTIFACT="$private_source" \
    CCLSH_IMAGE_ARTIFACT="$private_image" \
    CCLSH_INSTALL_DIRECTORY="$private_directory" \
    CCLSH_LOGIN_USER=nobody \
    CCLSH_SHELLS_FILE="$private_shells" \
    CCLSH_BUILD_ATTESTATION="$attestation" \
    scripts/install >/dev/null
    private_release=$(realpath "$private_shell")
    private_metadata=$(
        stat -c '%a:%u:%g:%n' \
            "$(dirname "$private_release")" \
            "$private_release" \
            "$private_release.image" \
            "$private_release.attestation" \
            "$private_release.login-uid"
    )
    : >"$private_failure"
    if CCLSH_SKIP_BUILD=1 \
       CCLSH_KERNEL_ARTIFACT="$private_source" \
       CCLSH_IMAGE_ARTIFACT="$private_image" \
       CCLSH_INSTALL_DIRECTORY="$private_directory" \
       CCLSH_LOGIN_USER=nobody \
       CCLSH_SHELLS_FILE="$private_shells" \
       CCLSH_BUILD_ATTESTATION="$attestation" \
       scripts/install >/dev/null 2>&1
    then
        echo "install accepted a failed reused-release probe" >&2
        exit 1
    fi
    if [ "$(realpath "$private_shell")" != "$private_release" ] ||
       [ "$(
           stat -c '%a:%u:%g:%n' \
               "$(dirname "$private_release")" \
               "$private_release" \
               "$private_release.image" \
               "$private_release.attestation" \
               "$private_release.login-uid"
         )" != "$private_metadata" ]
    then
        echo "failed reused release changed active metadata" >&2
        exit 1
    fi

    if CCLSH_SKIP_BUILD=1 \
       CCLSH_KERNEL_ARTIFACT="$private_source" \
       CCLSH_IMAGE_ARTIFACT="$private_image" \
       CCLSH_INSTALL_DIRECTORY="$private_directory" \
       CCLSH_SHELLS_FILE="$private_shells" \
       scripts/install >/dev/null 2>&1
    then
        echo "owner-only install replaced a registered login shell" >&2
        exit 1
    fi
    if CCLSH_SKIP_BUILD=1 \
       CCLSH_KERNEL_ARTIFACT="$private_source" \
       CCLSH_IMAGE_ARTIFACT="$private_image" \
       CCLSH_INSTALL_DIRECTORY="$private_directory/../private-bin" \
       scripts/install >/dev/null 2>&1
    then
        echo "install accepted a noncanonical destination" >&2
        exit 1
    fi
fi

scripts/register-shell "$shell_path" "$shells_file" >/dev/null
scripts/register-shell "$shell_path" "$shells_file" >/dev/null
if [ "$(id -u)" -eq 0 ] && id nobody >/dev/null 2>&1; then
    chmod 644 "$second_release.image"
    scripts/register-shell "$shell_path" "$shells_file" nobody >/dev/null
fi

if [ "$(grep -Fxc -- "$shell_path" "$shells_file")" -ne 1 ]; then
    echo "register-shell did not add the path exactly once" >&2
    exit 1
fi

concurrent_shells=$temporary_directory/concurrent-shells
printf '%s\n' /bin/sh >"$concurrent_shells"
chmod 640 "$concurrent_shells"
/usr/bin/timeout --kill-after=1 5 \
    scripts/register-shell "$shell_path" "$concurrent_shells" \
    >/dev/null &
first_registrar=$!
/usr/bin/timeout --kill-after=1 5 \
    scripts/register-shell "$shell_path" "$concurrent_shells" \
    >/dev/null &
second_registrar=$!
wait "$first_registrar"
wait "$second_registrar"
if [ "$(grep -Fxc -- "$shell_path" "$concurrent_shells")" -ne 1 ]; then
    echo "concurrent registration did not add the path exactly once" >&2
    exit 1
fi

if [ "$(stat -c %a "$shells_file")" != 640 ]; then
    echo "register-shell did not preserve the shells file mode" >&2
    exit 1
fi
if scripts/register-shell relative/cclsh "$shells_file" >/dev/null 2>&1; then
    echo "register-shell accepted a relative shell path" >&2
    exit 1
fi
if scripts/register-shell "$shell_path" \
     "$temporary_directory/./shells" >/dev/null 2>&1
then
    echo "register-shell accepted a noncanonical shells path" >&2
    exit 1
fi
if scripts/register-shell "$temporary_directory/not a shell" \
     "$shells_file" >/dev/null 2>&1
then
    echo "register-shell accepted whitespace in a shell path" >&2
    exit 1
fi
for timeout_setting in \
    CCLSH_PROBE_TIMEOUT=0 \
    CCLSH_PROBE_KILL_AFTER=invalid \
    CCLSH_LOCK_TIMEOUT=0
do
    timeout_name=${timeout_setting%%=*}
    timeout_value=${timeout_setting#*=}
    if env "$timeout_name=$timeout_value" \
         scripts/register-shell "$shell_path" "$shells_file" \
         >/dev/null 2>&1
    then
        echo "register-shell accepted invalid timeout: $timeout_setting" >&2
        exit 1
    fi
done
if [ "$(id -u)" -eq 0 ] &&
   scripts/register-shell "$shell_path" "$shells_file" root \
       >/dev/null 2>&1
then
    echo "register-shell accepted root as its probe user" >&2
    exit 1
fi

resolved_image=$second_release.image
if [ "$(id -u)" -eq 0 ] && id nobody >/dev/null 2>&1; then
    chmod 640 "$resolved_image"
    if scripts/register-shell "$shell_path" "$shells_file" nobody \
         >/dev/null 2>&1
    then
        echo "register-shell accepted an image unreadable by the probe user" >&2
        exit 1
    fi
fi
chmod 644 "$resolved_image"

chmod 775 "$second_release"
if scripts/register-shell "$shell_path" "$shells_file" >/dev/null 2>&1; then
    echo "register-shell accepted a writable shell artifact" >&2
    exit 1
fi
chmod 755 "$second_release"

sync_failure_shells=$temporary_directory/sync-failure-shells
sync_failure_expected=$temporary_directory/sync-failure.expected
sync_failure_marker=$temporary_directory/sync-failure.marker
sync_failure_bin=$temporary_directory/sync-failure-bin
printf '%s\n' /bin/sh >"$sync_failure_shells"
chmod 640 "$sync_failure_shells"
cp "$sync_failure_shells" "$sync_failure_expected"
mkdir "$sync_failure_bin"
real_sync=$(command -v sync)
cat >"$sync_failure_bin/sync" <<EOF
#!/bin/sh
if test "\${2:-}" = "$temporary_directory" &&
   test ! -e "$sync_failure_marker"
then
    : >"$sync_failure_marker"
    exit 1
fi
exec "$real_sync" "\$@"
EOF
chmod 755 "$sync_failure_bin/sync"
if PATH="$sync_failure_bin:$PATH" \
   scripts/register-shell "$shell_path" "$sync_failure_shells" \
       >/dev/null 2>&1
then
    echo "register-shell accepted a failed directory sync" >&2
    exit 1
fi
if ! cmp -s "$sync_failure_expected" "$sync_failure_shells" ||
   grep -Fqx -- "$shell_path" "$sync_failure_shells"
then
    echo "directory sync failure did not restore the shells file" >&2
    exit 1
fi
rm -f "$sync_failure_marker"
sync_failure_new_shells=$temporary_directory/sync-failure-new-shells
if PATH="$sync_failure_bin:$PATH" \
   scripts/register-shell "$shell_path" "$sync_failure_new_shells" \
       >/dev/null 2>&1
then
    echo "register-shell accepted a failed first directory sync" >&2
    exit 1
fi
if [ -e "$sync_failure_new_shells" ] ||
   [ -L "$sync_failure_new_shells" ]
then
    echo "first directory sync failure left a shells file" >&2
    exit 1
fi

if [ "$(id -u)" -eq 0 ] && id nobody >/dev/null 2>&1; then
    unsafe_registry=$temporary_directory/unsafe-registry
    mkdir "$unsafe_registry"
    chown nobody:$(id -g nobody) "$unsafe_registry"
    if scripts/register-shell "$shell_path" "$unsafe_registry/shells" \
         >/dev/null 2>&1
    then
        echo "register-shell accepted an untrusted registry directory" >&2
        exit 1
    fi
    if find "$unsafe_registry" -mindepth 1 -print -quit | grep -q .; then
        echo "rejected registry directory was modified" >&2
        exit 1
    fi

    safe_registry=$temporary_directory/safe-registry
    unsafe_lock_directory=$temporary_directory/unsafe-locks
    mkdir "$safe_registry" "$unsafe_lock_directory"
    chown nobody:$(id -g nobody) "$unsafe_lock_directory"
    printf '%s\n' /bin/sh >"$safe_registry/shells"
    chmod 640 "$safe_registry/shells"
    inherited_tmp_shells=$safe_registry/inherited-tmp-shells
    printf '%s\n' /bin/sh >"$inherited_tmp_shells"
    chmod 640 "$inherited_tmp_shells"
    if ! TMPDIR="$unsafe_registry/missing" \
         scripts/register-shell "$shell_path" "$inherited_tmp_shells" \
             >/dev/null
    then
        echo "root registration trusted an inherited temporary directory" >&2
        exit 1
    fi
    if [ "$(grep -Fxc -- "$shell_path" "$inherited_tmp_shells")" -ne 1 ]; then
        echo "root registration with an unsafe TMPDIR did not complete" >&2
        exit 1
    fi
    unsafe_shells=$safe_registry/unsafe-shells
    printf '%s\n' /bin/sh >"$unsafe_shells"
    chmod 664 "$unsafe_shells"
    chown nobody:$(id -g nobody) "$unsafe_shells"
    unsafe_shells_metadata=$(stat -c '%a:%u:%g' "$unsafe_shells")
    if scripts/register-shell "$shell_path" "$unsafe_shells" \
         >/dev/null 2>&1
    then
        echo "register-shell accepted an untrusted shells file" >&2
        exit 1
    fi
    if [ "$(stat -c '%a:%u:%g' "$unsafe_shells")" != \
         "$unsafe_shells_metadata" ] ||
       grep -Fqx -- "$shell_path" "$unsafe_shells"
    then
        echo "rejected shells file was modified" >&2
        exit 1
    fi
    if CCLSH_SHELLS_LOCK_FILE="$unsafe_lock_directory/lock" \
       scripts/register-shell "$shell_path" "$safe_registry/shells" \
           >/dev/null 2>&1
    then
        echo "register-shell accepted an untrusted lock directory" >&2
        exit 1
    fi
    if grep -Fqx -- "$shell_path" "$safe_registry/shells" ||
       find "$unsafe_lock_directory" -mindepth 1 -print -quit | grep -q .
    then
        echo "rejected lock directory changed registration state" >&2
        exit 1
    fi

    unsafe_lock=$safe_registry/unsafe.lock
    printf '%s\n' unchanged >"$unsafe_lock"
    chmod 664 "$unsafe_lock"
    chown nobody:$(id -g nobody) "$unsafe_lock"
    unsafe_lock_metadata=$(stat -c '%a:%u:%g' "$unsafe_lock")
    if CCLSH_SHELLS_LOCK_FILE="$unsafe_lock" \
       scripts/register-shell "$shell_path" "$safe_registry/shells" \
           >/dev/null 2>&1
    then
        echo "register-shell accepted an untrusted lock file" >&2
        exit 1
    fi
    if [ "$(stat -c '%a:%u:%g' "$unsafe_lock")" != \
         "$unsafe_lock_metadata" ] ||
       [ "$(cat "$unsafe_lock")" != unchanged ] ||
       grep -Fqx -- "$shell_path" "$safe_registry/shells"
    then
        echo "rejected lock file was modified" >&2
        exit 1
    fi
fi

concurrent_directory=$temporary_directory/concurrent-bin
concurrent_one=$temporary_directory/concurrent-one
concurrent_two=$temporary_directory/concurrent-two
concurrent_image_one=$temporary_directory/concurrent-one.image
concurrent_image_two=$temporary_directory/concurrent-two.image
cat >"$concurrent_one" <<'EOF'
#!/bin/sh
case "${1:-}" in --version|-c) exit 0;; *) exit 2;; esac
EOF
cat >"$concurrent_two" <<'EOF'
#!/bin/sh
case "${1:-}" in --version|-c) exit 0;; *) exit 2;; esac
# distinct release
EOF
chmod 755 "$concurrent_one" "$concurrent_two"
printf '%s\n' 'first concurrent image' >"$concurrent_image_one"
printf '%s\n' 'second concurrent image' >"$concurrent_image_two"
/usr/bin/timeout --kill-after=1 40 env \
    CCLSH_SKIP_BUILD=1 \
    CCLSH_KERNEL_ARTIFACT="$concurrent_one" \
    CCLSH_IMAGE_ARTIFACT="$concurrent_image_one" \
    CCLSH_INSTALL_DIRECTORY="$concurrent_directory" \
    scripts/install >/dev/null &
first_installer=$!
/usr/bin/timeout --kill-after=1 40 env \
    CCLSH_SKIP_BUILD=1 \
    CCLSH_KERNEL_ARTIFACT="$concurrent_two" \
    CCLSH_IMAGE_ARTIFACT="$concurrent_image_two" \
    CCLSH_INSTALL_DIRECTORY="$concurrent_directory" \
    scripts/install >/dev/null &
second_installer=$!
wait "$first_installer"
wait "$second_installer"
if [ ! -x "$concurrent_directory/cclsh" ]; then
    echo "concurrent installs left no active shell" >&2
    exit 1
fi
concurrent_active=$(realpath -e "$concurrent_directory/cclsh")
concurrent_active_image=$(realpath -e "$concurrent_directory/cclsh.image")
if cmp -s "$concurrent_active" "$concurrent_one"; then
    concurrent_expected_image=$concurrent_image_one
elif cmp -s "$concurrent_active" "$concurrent_two"; then
    concurrent_expected_image=$concurrent_image_two
else
    echo "concurrent installs activated an unknown kernel" >&2
    exit 1
fi
if [ "$concurrent_active_image" != "$concurrent_active.image" ] ||
   ! cmp -s "$concurrent_active.image" "$concurrent_expected_image"
then
    echo "concurrent installs activated a mixed kernel and image" >&2
    exit 1
fi
if find "$concurrent_directory/.cclsh-releases" -mindepth 2 \
     -type d -print -quit | grep -q .
then
    echo "concurrent installs nested a staging directory in a release" >&2
    exit 1
fi

lock_ready=$temporary_directory/build-lock-ready
lock_release=$temporary_directory/build-lock-release
mkfifo "$lock_ready" "$lock_release"
scripts/with-build-lock \
    sh -c 'printf "ready\n" >"$1"; IFS= read -r answer <"$2"' \
    sh "$lock_ready" "$lock_release" &
lock_holder=$!
IFS= read -r ready <"$lock_ready"
if CCLSH_BUILD_LOCK_PATH="$PWD/.cclsh-build.lock" \
     scripts/build-lock-held
then
    echo "build lock accepted a spoofed ownership variable" >&2
    exit 1
fi
if CCLSH_BUILD_LOCK_TIMEOUT=0.1 scripts/with-build-lock true \
     >/dev/null 2>&1
then
    echo "build lock admitted a concurrent publisher" >&2
    exit 1
fi
printf 'release\n' >"$lock_release"
wait "$lock_holder"
lock_holder=

echo "Installation tooling checks passed."
