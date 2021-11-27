; ModuleID = 'main'


 


define external ccc  i8* @__eddaab7dd3adaa6b7d769751eb6c3b5e__Identity(i8* )    {
  %2 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64, i8*}* getelementptr inbounds ({i64, i8*}, {i64, i8*}* inttoptr (i32 0 to {i64, i8*}*), i32 1) to i64))  
  %3 = bitcast i8* %2 to {i64, i8*}* 
  %4 = getelementptr  {i64, i8*}, {i64, i8*}* %3, i32 0, i32 1 
  store  i8* %0, i8** %4, align 8 
  %5 = getelementptr  {i64, i8*}, {i64, i8*}* %3, i32 0, i32 0 
  store  i64 0, i64* %5, align 8 
  %6 = bitcast {i64, i8*}* %3 to i8* 
  ret i8* %6 
}


define external ccc  i8* @$Functor$Identity$map(i8*  %f_0, i8*  %m_0)    {
; <label>:0:
  %1 = bitcast i8* %f_0 to {i8*, i32, i32, i8*}* 
  %2 = bitcast i8* %m_0 to i8* 
  %3 = bitcast {i8*, i32, i32, i8*}* %1 to i8* 
  %4 = bitcast i8* %2 to {i64, i8*}* 
  %5 = getelementptr  {i64, i8*}, {i64, i8*}* %4, i32 0, i32 1 
  %6 = getelementptr  {i64, i8*}, {i64, i8*}* %4, i32 0, i32 0 
  %7 = load  i64, i64* %6, align 8 
  %8 = icmp eq i64 0, %7 
  %9 = load  i8*, i8** %5, align 8 
  %10 = bitcast i8* %9 to i8* 
  %11 = and i1 1, 1 
  %12 = and i1 %8, %11 
  br i1 %12, label %branchExpBlock_0, label %exitBlock_0 
branchExpBlock_0:
  %13 = bitcast i8* %2 to {i64, i8*}* 
  %14 = getelementptr  {i64, i8*}, {i64, i8*}* %13, i32 0, i32 1 
  %15 = load  i8*, i8** %14, align 8 
  %16 = bitcast i8* %15 to i8* 
  br label %exitBlock_0 
exitBlock_0:
  %17 = phi i8* [%16, %branchExpBlock_0], [zeroinitializer, %0] 
  %18 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %3, i32  1, i8*  %17)  
  %19 = bitcast i8* %18 to i8* 
  %20 =  call ccc  i8*  @__eddaab7dd3adaa6b7d769751eb6c3b5e__Identity(i8*  %19)  
  %21 = bitcast i8* %20 to i8* 
  ret i8* %21 
}


@$Functor$Identity =    global {{i8*, i32, i32}} { {i8*, i32, i32} { i8* bitcast (i8* (i8*, i8*)* @$Functor$Identity$map to i8*), i32 2, i32 2 } }


define external ccc  i8* @$Applicative$Identity$ap(i8*  %mf_0, i8*  %mm_0)    {
  %1 = bitcast i8* %mf_0 to i8* 
  %2 = bitcast i8* %mm_0 to i8* 
  %3 =  call ccc  i8*  @__eddaab7dd3adaa6b7d769751eb6c3b5e__runIdentity(i8*  %1)  
  %4 =  call ccc  i8*  @__eddaab7dd3adaa6b7d769751eb6c3b5e__runIdentity(i8*  %2)  
  %5 = bitcast i8* %4 to i8* 
  %6 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %3, i32  1, i8*  %5)  
  %7 = bitcast i8* %6 to i8* 
  %8 =  call ccc  i8*  @__eddaab7dd3adaa6b7d769751eb6c3b5e__Identity(i8*  %7)  
  %9 = bitcast i8* %8 to i8* 
  ret i8* %9 
}


define external ccc  i8* @$Applicative$Identity$pure(i8*  %a_0)    {
  %1 = bitcast i8* %a_0 to i8* 
  %2 =  call ccc  i8*  @__eddaab7dd3adaa6b7d769751eb6c3b5e__Identity(i8*  %1)  
  %3 = bitcast i8* %2 to i8* 
  ret i8* %3 
}

@$Applicative$Identity =    global {{i8*, i32, i32}, {i8*, i32, i32}} { {i8*, i32, i32} { i8* bitcast (i8* (i8*, i8*)* @$Applicative$Identity$ap to i8*), i32 2, i32 2 }, {i8*, i32, i32} { i8* bitcast (i8* (i8*)* @$Applicative$Identity$pure to i8*), i32 1, i32 1 } }


define external ccc  i8* @anonymous$lifted$0(i8*  %$Applicative$s44_0, i8*  %x_0)    {
  %1 = bitcast i8* %$Applicative$s44_0 to i8* 
  %2 = bitcast i8* %x_0 to i8* 
  %3 = bitcast i8* %1 to {{i8*, i32, i32}, {i8*, i32, i32}}* 
  %4 = getelementptr  {{i8*, i32, i32}, {i8*, i32, i32}}, {{i8*, i32, i32}, {i8*, i32, i32}}* %3, i32 0, i32 1 
  %5 = bitcast {i8*, i32, i32}* %4 to i8* 
  %6 = bitcast {i8*, i8*}* zeroinitializer to i8* 
  %7 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i8*}* getelementptr inbounds ({i8*, i8*}, {i8*, i8*}* inttoptr (i32 0 to {i8*, i8*}*), i32 1) to i64))  
  %8 = bitcast i8* %7 to {i8*, i8*}* 
  %9 = getelementptr  {i8*, i8*}, {i8*, i8*}* %8, i32 0, i32 0 
  store  i8* %2, i8** %9, align 8 
  %10 = getelementptr  {i8*, i8*}, {i8*, i8*}* %8, i32 0, i32 1 
  store  i8* %6, i8** %10, align 8 
  %11 = bitcast {i8*, i8*}* %8 to i8* 
  %12 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %5, i32  1, i8*  %11)  
  %13 = bitcast i8* %12 to i8* 
  ret i8* %13 
}


define external ccc  i8* @__eddaab7dd3adaa6b7d769751eb6c3b5e__runIdentity(i8*  %__x___0)    {
; <label>:0:
  %1 = bitcast i8* %__x___0 to i8* 
  %2 = bitcast i8* %1 to {i64, i8*}* 
  %3 = getelementptr  {i64, i8*}, {i64, i8*}* %2, i32 0, i32 1 
  %4 = getelementptr  {i64, i8*}, {i64, i8*}* %2, i32 0, i32 0 
  %5 = load  i64, i64* %4, align 8 
  %6 = icmp eq i64 0, %5 
  %7 = load  i8*, i8** %3, align 8 
  %8 = bitcast i8* %7 to i8* 
  %9 = and i1 1, 1 
  %10 = and i1 %6, %9 
  br i1 %10, label %branchExpBlock_0, label %exitBlock_0 
branchExpBlock_0:
  %11 = bitcast i8* %1 to {i64, i8*}* 
  %12 = getelementptr  {i64, i8*}, {i64, i8*}* %11, i32 0, i32 1 
  %13 = load  i8*, i8** %12, align 8 
  %14 = bitcast i8* %13 to i8* 
  br label %exitBlock_0 
exitBlock_0:
  %15 = phi i8* [%14, %branchExpBlock_0], [zeroinitializer, %0] 
  ret i8* %15 
}


define external ccc  i8* @__eddaab7dd3adaa6b7d769751eb6c3b5e__pure3(i8*  %$Functor$s44_0, i8*  %$Applicative$s44_0)    {
  %1 = bitcast i8* %$Functor$s44_0 to i8* 
  %2 = bitcast i8* %$Applicative$s44_0 to i8* 
  %3 = bitcast i8* (i8*, i8*)* @anonymous$lifted$0 to i8* 
  %4 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*}* getelementptr inbounds ({i8*}, {i8*}* inttoptr (i32 0 to {i8*}*), i32 1) to i64))  
  %5 = bitcast i8* %4 to {i8*}* 
  %6 = getelementptr  {i8*}, {i8*}* %5, i32 0, i32 0 
  store  i8* %2, i8** %6, align 8 
  %7 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32, i8*}* getelementptr inbounds ({i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* inttoptr (i32 0 to {i8*, i32, i32, i8*}*), i32 1) to i64))  
  %8 = bitcast i8* %7 to {i8*, i32, i32, i8*}* 
  %9 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %8, i32 0, i32 0 
  store  i8* %3, i8** %9, align 8 
  %10 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %8, i32 0, i32 1 
  store  i32 2, i32* %10, align 8 
  %11 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %8, i32 0, i32 2 
  store  i32 1, i32* %11, align 8 
  %12 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %8, i32 0, i32 3 
  store  i8* %4, i8** %12, align 8 
  %13 = bitcast {i8*, i32, i32, i8*}* %8 to i8* 
  ret i8* %13 
}


declare external ccc  i8* @__applyPAP__(i8*, i32, ...)    


declare external ccc  i8* @malloc(i64)    


declare external ccc  i8* @GC_malloc(i64)    


declare external ccc  i8* @calloc(i32, i32)    


declare external ccc  i1 @__streq__(i8*, i8*)    


declare external ccc  i8* @__strConcat__(i8*, i8*)    


declare external ccc  i1 @MadList_hasMinLength(double, {i8*, i8*}*)    


declare external ccc  i1 @MadList_hasLength(double, {i8*, i8*}*)    


declare external ccc  {i8*, i8*}* @MadList_singleton(i8*)    


declare external ccc  {i8*, i8*}* @__MadList_push__(i8*, {i8*, i8*}*)    


declare external ccc  {i8*, i8*}* @MadList_concat({i8*, i8*}*, {i8*, i8*}*)    


@__eddaab7dd3adaa6b7d769751eb6c3b5e__tchouk2 =    global i8* undef


define external ccc  void @main()    {
entry_0:
  %0 = bitcast i8* (i8*, i8*)* @__eddaab7dd3adaa6b7d769751eb6c3b5e__pure3 to i8* 
  %1 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32}* getelementptr inbounds ({i8*, i32, i32}, {i8*, i32, i32}* inttoptr (i32 0 to {i8*, i32, i32}*), i32 1) to i64))  
  %2 = bitcast i8* %1 to {i8*, i32, i32}* 
  %3 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %2, i32 0, i32 0 
  store  i8* %0, i8** %3, align 8 
  %4 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %2, i32 0, i32 1 
  store  i32 2, i32* %4, align 8 
  %5 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %2, i32 0, i32 2 
  store  i32 2, i32* %5, align 8 
  %6 = bitcast {i8*, i32, i32}* %2 to i8* 
  %7 = bitcast {{i8*, i32, i32}}* @$Functor$Identity to i8* 
  %8 = bitcast {{i8*, i32, i32}, {i8*, i32, i32}}* @$Applicative$Identity to i8* 
  %9 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %6, i32  2, i8*  %7, i8*  %8)  
  %10 = bitcast i8* %9 to {i8*, i32, i32, i8*}* 
  %11 = bitcast {i8*, i32, i32, i8*}* %10 to i8* 
  %12 =  call ccc  i8*  @GC_malloc(i64  3)  
  %13 = getelementptr  i8, i8* %12, i32 0 
  store  i8 51, i8* %13, align 8 
  %14 = getelementptr  i8, i8* %12, i32 1 
  store  i8 51, i8* %14, align 8 
  %15 = getelementptr  i8, i8* %12, i32 2 
  store  i8 0, i8* %15, align 8 
  %16 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %11, i32  1, i8*  %12)  
  %17 = bitcast i8* %16 to i8* 
  store  i8* %17, i8** @__eddaab7dd3adaa6b7d769751eb6c3b5e__tchouk2, align 8 
  ret void 
}