while read l
do
  echo "Line: $l"
done

if true; then
  echo "true"
fi
if false; then
  echo "won't happen"
fi