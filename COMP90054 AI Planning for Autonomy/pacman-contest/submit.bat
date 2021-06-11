@ECHO OFF

ECHO Deleting old tag ...
git push --delete origin submission-contest
git tag --delete submission-contest

ECHO Re-tag ...
git tag submission-contest

ECHO Push to remote...
git tag -l
git push origin --tags

PAUSE
