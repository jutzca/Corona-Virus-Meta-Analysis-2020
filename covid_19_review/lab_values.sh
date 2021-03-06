echo "Assumption: right-skewed normal distribution"
echo
echo

for VAL in "Lymphocyte" "Leucocytes" "Neutrophils" "AST" "ALT" "D_Dimer" "IL6"; do
  poetry run python simulate_skew_normal_distribution.py -i ../data/Corona_review_labor_edited.csv -c ${VAL}_Mean --alpha0 0 --alpha1 10000
  echo
done

echo "Assumption: left-skewed normal distribution"
echo
echo

for VAL in "Lymphocyte" "Leucocytes" "Neutrophils" "AST" "ALT" "D_Dimer" "IL6"; do
  poetry run python simulate_skew_normal_distribution.py -i ../data/Corona_review_labor_edited.csv -c ${VAL}_Mean --alpha0 -100000 --alpha1 0
  echo
done
