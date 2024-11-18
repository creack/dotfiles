mkdir -p ~/projects/gravitational
cd ~/projects/gravitational
git clone git@github.com:gravitational/teleport.git
echo 22 > teleport/.nvmrc
cd teleport
make init-submodules-e
make rustup-install-target-toolchain
time make full-ent
