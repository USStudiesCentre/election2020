# Analysis for 2020 primaries


## Instructions for cloning the repository to your local machine

1. Head to the primaries folder on your local machine in Terminal.
2. Type (or copy) `git clone https://github.com/usstudiescentre/election2020.git`


## Instructions for pushing and pulling data from GitHub:

1. To pull data from GitHub, open Terminal and find the primaries folder on your local machine. Type `git pull` to download any changes added to the remote repository to your local copy. If there are no changes, your terminal should display the following message: `Already up to date.` If there are any changes, the edited files will be noted in a list. Make sure to do this before every session.


2. To push data from your local machine, run the following commands:
* git add specificfile.R OR git add . if you want to add all files in the repository
* git commit -m 'This should be an informative message - edits to specificfile.R for blah blah'
* git push origin


## Instructions for switching branches on GitHub:

Sometimes you want to work from another branch in the remote repository -- say a branch called `dev` instead of `master`. To do this, create the new branch on GitHub then inside your Terminal window type `git checkout dev`. All pulling and pushing will go to the new branch and won't affect the `master` branch. This is useful when `master` is used for production purposes. To check which branch you are working in, run `git branch` and you will get a list of all branches associated with the repo; the one in use will be highlighted. 





