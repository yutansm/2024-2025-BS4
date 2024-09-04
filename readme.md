# Guide
近本的に1 wayのsyncだと思って欲しい。このパソコン => github => Kyoto 
`.ps`ファイルに関しては特別扱い。Kyotoで作成された図は Kyoto => local のsyncあり。こうすることで，スーパーパソコンを製図に使える。それ以外は全部ローカルでしてね。はい

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
- Code execution (generate ps files)
- File management

Kyoto:
- Code execution (generate ps files)

## What files get synced at git push?
When : at `git push`

### Local to Github and Kyoto:
What : Everything
See : .github/workflows/deploy.yml

### Kyoto to Local:
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

## Q and A
