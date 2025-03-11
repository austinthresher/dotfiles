#!/bin/bash
set -e
if [ ! -d "$HOME/.pyenv" ]; then
	curl -L https://github.com/pyenv/pyenv-installer/raw/master/bin/pyenv-installer | bash
	cat >> ~/.bashrc_local <<-'EOF'

	# configure pyenv
	export PYENV_ROOT="$HOME/.pyenv"
	[[ -d "$PYENV_ROOT/bin" ]] && export PATH="$PYENV_ROOT/bin:$PATH"
	eval "$(pyenv init - bash)"
	eval "$(pyenv virtualenv-init - bash)"

	EOF
	echo 'Done. Make sure to install the build dependencies for Python.'
	echo 'Note that `apt-get build-dep python3` appears to miss development'
	echo 'libraries for readline, openssl, bzip2, and sqlite3.'
else
	echo "$HOME/.pyenv already exists"
fi

