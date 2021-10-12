; ModuleID = 'main'


 


declare external ccc  i32 @puts(i8*)    


declare external ccc  i8* @malloc(i64)    


define external ccc  void @main()    {
entry_0:
  %0 =  call ccc  i8*  @malloc(i64  2)  
  %1 = getelementptr  i8, i8* %0, i32 0 
  store  i8 49, i8* %1, align 8 
  %2 = getelementptr  i8, i8* %0, i32 1 
  store  i8 0, i8* %2, align 8 
  %3 = alloca {i8*, double}, align 8 
  %4 = getelementptr  {i8*, double}, {i8*, double}* %3, i32 0, i32 0 
  store  i8* %0, i8** %4, align 8 
  %5 = getelementptr  {i8*, double}, {i8*, double}* %3, i32 0, i32 1 
  store  double 1.000000e0, double* %5, align 8 
  %6 = alloca {i8*, double}*, align 4 
  store  {i8*, double}* %3, {i8*, double}** %6, align 4 
  %7 = load  {i8*, double}*, {i8*, double}** %6, align 4 
  %8 = getelementptr  {i8*, double}, {i8*, double}* %7, i32 0, i32 0 
  %9 = getelementptr  {i8*, double}, {i8*, double}* %7, i32 0, i32 1 
  %10 = icmp eq i1 1, 1 
  br i1 %10, label %branchExpBlock_0, label %exitBlock_0 
branchExpBlock_0:
  %11 = load  i8*, i8** %8, align 4 
  br label %exitBlock_0 
exitBlock_0:
  %12 = phi i8* [%11, %branchExpBlock_0] 
  %13 = alloca i8*, align 4 
  store  i8* %12, i8** %13, align 4 
  ret void 
}