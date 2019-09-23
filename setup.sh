set -e

cleanup() {
    trap true INT TERM EXIT
    rm -rf "$install_dir"
    echo "Cleaning up."
    echo "Done."
}

echo "===> Creating build directory."
install_dir=$(mktemp -d)

echo "===> Setting up exit trap."
trap cleanup INT TERM EXIT

echo "===> Fetching source from GitHub."
curl -L https://github.com/robwhitaker/taskwarrior-habitica-bridge/tarball/master | tar xz -C "$install_dir"

echo "===> Moving into build directory."
cd "$install_dir"
cd "$(ls -d */ | head -n 1)"

echo "===> Installing."
nix-env -i -f ./.
