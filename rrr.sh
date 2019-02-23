git pull
R CMD BATCH tmp.R
cat 'log.txt'
git add res*.png
git commit . -m "a"
git push
