# READ ME 

To install on a remote machine: 
1. make sure you alias to your *.zshrc*
`alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'`
2. your source repo ignores the folder where you'll clone it
`echo ".cfg" >> .gitignore`
3. clone you 'dot' folders:
`git clone --bare <git-repo-url> $HOME/.cfg`
4. checkout 
`config checkout`

