# Guide
`git push` をすることで
ギットハブと京都は`git push`イベントでシンクされる（.psファイルを除く）

## Commands
### See current status
`git status`

### Add files to commit
`git add {type file name here}`
or 
`git add -A` to add all files to commit

### Commit files
`git commit -m "commit message here"`

### Push commit to github
`git push`

## What you can do 
注意！！！　サーバーではファイルを動かさないように！Duplicateができてしまう。

Local:
- Software development
- Code execution
- File management

Kyoto:
- Code execution

## When (and what) files get synced?

### Local to Github and Kyoto:
When : at `git push`
What : Everything except `.ps` files
See : .github/workflows/deploy.yml

### Kyoto to Local:
When : at `git commit'
What : only `.ps` files 

# What the hell is going on in this boomdabbo kind of world that we were forced to live in as young children of the Republic?
1. File is saved
2. File is committed
3. Pre-commit hook is triggered. New .ps files from Kyoto are synced locally
4. Commit is pushed
5. Github is synced with local. Github syncs with Kyoto copying all files except .ps files. 

## Pre-commit hook
1. The pre-commit hook is triggered by the git commit command.
2. The pre-commit hook looks for 