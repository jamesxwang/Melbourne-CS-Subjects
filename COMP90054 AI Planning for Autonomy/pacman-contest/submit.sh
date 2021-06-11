#!/usr/bin/env bash
git push --delete origin submission-contest
git tag --delete submission-contest
git tag submission-contest
git tag -l
git push origin --tags

