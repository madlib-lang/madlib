while [ true ]
do
  rm -r .module_cache
  GC_MARKERS=1 GC_NPROCS=1 GC_FREE_SPACE_DIVISOR=1000 lldb --batch ./build/a.out -o run
done
