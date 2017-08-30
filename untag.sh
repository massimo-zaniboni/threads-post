
awk "/$2-begin/{flag=1;next}/$2-end/{flag=0}flag" $1
