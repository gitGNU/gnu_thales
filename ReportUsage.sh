#!/bin/sh

echo "I'd like to collect some infos about who is using thales. This will only"
echo "take a few seconds to answer the questions."
echo
echo "Do you agree to answer them now [Y/N] ? (you can answer them later by"
echo "running ./ReportUsage.sh in thales' directory)."
echo -n "[Y] > "
read answer
if [ "X$answer" = "XN" -o "X$answer" = "Xn" ]; then
	exit 0
fi

echo "On which network are you using thales ? Please give exact name, so that I"
echo "can eventually connect and say hi !"
echo -n "> "
read network
echo
echo "Which ircd are you using ?"
echo -n "> "
read ircd
echo
echo "What is the main language used on your network ?"
echo -n "> "
read lang
echo
echo "What is the approximate number of users on your network ?"
echo -n "> "
read size
echo
echo "What is your email address ? (you don't _need_ to answer this one)"
echo -n "> "
read email

version=`cat configure.in |grep AM_INIT_AUTOMAKE\(thales |sed -e 's/.*thales, \(.*\))/\1/'`
cat > mail.txt << ENDOFFILE
THALES USAGE

Network : $network
Ircd : $ircd
Language : $lang
Size : $size
Email : $email
Version : $version

Please mail this file to thales-usage@lucas-nussbaum.net
ENDOFFILE

echo "OK, that's all. Here are the results, saved in mail.txt :"
echo "--------------"
cat mail.txt
echo "--------------"
echo "Do you want me to mail the file using the standard unix 'mail'"
echo "command ? [Y/N]"
echo -n "[Y] > "
read answer
if [ "X$answer" = "XN" -o "X$answer" = "Xn" ]; then
	echo "OK, please mail the content of mail.txt to thales-usage@lucas-nussbaum.net"
	exit 0
fi

cat mail.txt |mail -s "thales usage" thales-usage@lucas-nussbaum.net
echo "mail sent, thanks !"
rm mail.txt
echo
