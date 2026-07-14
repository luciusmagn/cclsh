#!/bin/sh
# Check the fail-closed kernel argument-boundary probe.
set -eu
cd "$(dirname "$0")/.."

temporary_directory=$(
    mktemp -d "${TMPDIR:-/tmp}/cclsh-argument-boundary-check.XXXXXX"
)
cleanup()
{
    rm -rf -- "$temporary_directory"
}
trap cleanup 0
trap 'exit 129' 1
trap 'exit 130' 2
trap 'exit 143' 15

image=$temporary_directory/cclsh.image
good_kernel=$temporary_directory/good-kernel
bad_kernel=$temporary_directory/bad-kernel
printf '%s\n' image >"$image"

sed 's|@IMAGE@|'"$image"'|g' >"$good_kernel" <<'EOF'
#!/bin/sh
if [ "$#" -eq 4 ] &&
   [ "$1" = -I ] &&
   [ "$2" = @IMAGE@ ] &&
   [ "$3" = -c ] &&
   [ "$4" = --no-avx ]
then
    echo "cclsh: --no-avx: command not found" >&2
    exit 127
fi
exit 2
EOF
printf '%s\n' '#!/bin/sh' 'exit 2' >"$bad_kernel"
chmod 755 "$good_kernel" "$bad_kernel"

scripts/verify-argument-boundary "$good_kernel" "$image"
if scripts/verify-argument-boundary "$bad_kernel" "$image" \
     >/dev/null 2>&1
then
    echo "argument boundary accepted a kernel that consumed the operand" >&2
    exit 1
fi
if CCLSH_BUILD_VALIDATION_TIMEOUT=0 \
     scripts/verify-argument-boundary "$good_kernel" "$image" \
     >/dev/null 2>&1
then
    echo "argument boundary accepted a disabled timeout" >&2
    exit 1
fi

echo "Argument-boundary tooling checks passed."
