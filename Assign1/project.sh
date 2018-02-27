#!/bin/bash
echo "Are you ready to run the script? Please type 'script_help' for introducing the function"


dir_status(){
git status
echo "finished"
}

uncommitted_rd(){
git diff>changes.log
echo "finished"
read -p "Do you want to see the uncommitted changes? (y/n) " ans
if [ "$ans" == "y" ];then
     Content=$(cat changes.log)
     printf "START OF FILE\n$Content\nEND OF FILE\n"
fi
#the "$ans" makes the function defined when nothing is typed, so it represents a string "" which is not equal to "y". Without the "", it has a potential error.
}

todo_rd(){
grep -r "#TODO" * > todo.log
echo "finished"
read -p "Do you want to see the todo.log? (y/n) " ans
if [ "$ans" == "y" ];then
     contents=$(cat todo.log)
     printf "START OF FILE\n$contents\nEND OF FILE\n"
fi
}

error_rd(){
echo "checking the syntax error and put the result into error.log"
find . -name "*.hs" -type f | xargs ghc -fno-code &>error.log
echo "checking finished"
read -p "Do you want to see the error.log? (y/n) " ans
if [ "$ans" == "y" ];then
     Contents=$(cat error.log)
     printf "START OF FILE\n$Contents\nEND OF FILE\n"
fi
}

script_help(){
printf "dir_status\nuncommitted_rd\ntodo_rd\nerror_rd\nnum_file\nnum_dr\nls -d */\n"
}

num_file(){
echo "There are $(find . type f | wc -l) files in this project."
}
#This feature is from https://github.com/aksamitn/CS1XA3/blob/master/ProjectAnalyze.sh 

num_dr(){
echo"There are $(find . -type d | ec -l) directories in this project."
}
#This feature is from https://github.com/aksamitn/CS1XA3/blob/master/ProjectAnalyze.sh

ls_dr(){
ls -d */
}
#list only the subdirectories when you have to many files in you current directory.


