; ModuleID = 'main'


 


; @__e3e8c77e8e733398f092072b275bef7e__yellow = external   global {i8*, i32, i32, i8*}* 


; declare external ccc  i8* @__61ebaf0b80ceb01f7afef988602b6f1f__push(i8*)    


; @$Functor$List = external   global {{i8*, i32, i32, i8*}} 


; declare external ccc  i8* @$Functor$List$map(i8*, i8*)    


; @$Monoid$List = external   global {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}} 


; declare external ccc  i8* @$Monoid$List$mappend(i8*, i8*)    


; declare external ccc  i8* @$Monoid$List$mempty()    


; @$Number$Byte = external   global {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}} 


; declare external ccc  i8* @"$Number$Byte$*"(i8*, i8*)    


; declare external ccc  i8* @"$Number$Byte$+"(i8*, i8*)    


; declare external ccc  i8* @$Number$Byte$-(i8*, i8*)    


; declare external ccc  i8* @$Number$Byte$__coerceNumber__(i8*)    


; @$Number$Float = external   global {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}} 


; declare external ccc  i8* @"$Number$Float$*"(i8*, i8*)    


; declare external ccc  i8* @"$Number$Float$+"(i8*, i8*)    


; declare external ccc  i8* @$Number$Float$-(i8*, i8*)    


; declare external ccc  i8* @$Number$Float$__coerceNumber__(i8*)    


; @$Number$Integer = external   global {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}} 


; declare external ccc  i8* @"$Number$Integer$*"(i8*, i8*)    


; declare external ccc  i8* @"$Number$Integer$+"(i8*, i8*)    


; declare external ccc  i8* @$Number$Integer$-(i8*, i8*)    


; declare external ccc  i8* @$Number$Integer$__coerceNumber__(i8*)    


; @$PrettyShow$List = external   global {{i8*, i32, i32, i8*}} 


; declare external ccc  i8* @$PrettyShow$List$ppShow(i8*, i8*)    


; @$Semigroup$List = external   global {{i8*, i32, i32, i8*}} 


; declare external ccc  i8* @$Semigroup$List$assoc(i8*, i8*)    


; @$Show$List = external   global {{i8*, i32, i32, i8*}} 


; declare external ccc  i8* @$Show$List$show(i8*)    


define external ccc  i8* @$Show$Byte$show(i8* )    {
  %2 = bitcast i8* (i8*)* @__7e60b3743245bd79b1de9c1b618cbcce__showByte to i8* 
  %3 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32, i8*}* getelementptr inbounds ({i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* inttoptr (i32 0 to {i8*, i32, i32, i8*}*), i32 1) to i64))  
  %4 = bitcast i8* %3 to {i8*, i32, i32, i8*}* 
  %5 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %4, i32 0, i32 0 
  store  i8* %2, i8** %5, align 8 
  %6 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %4, i32 0, i32 1 
  store  i32 1, i32* %6, align 8 
  %7 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %4, i32 0, i32 2 
  store  i32 1, i32* %7, align 8 
  %8 = bitcast {i8*, i32, i32, i8*}* %4 to i8* 
  %9 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %8, i32  1, i8*  %0)  
  ret i8* %9 
}


@$Show$Byte =    global {{i8*, i32, i32, i8*}} { {i8*, i32, i32, i8*} { i8* bitcast (i8* (i8*)* @$Show$Byte$show to i8*), i32 1, i32 1, i8* undef } }


define external ccc  i8* @$PrettyShow$Byte$ppShow(i8*  %_P__0)    {
  %1 = bitcast i8* %_P__0 to i8* 
  %2 = load  i8, i8* %1, align 8 
  %3 = load  {i8*, i32, i32, i8*}*, {i8*, i32, i32, i8*}** @__e3e8c77e8e733398f092072b275bef7e__yellow, align 8 
  %4 = bitcast {i8*, i32, i32, i8*}* %3 to i8* 
  %5 =  call ccc  i8*  @$Show$Byte$show(i8*  %_P__0)  
  %6 = bitcast i8* %5 to i8 addrspace(1)** 
  %7 = load  i8 addrspace(1)*, i8 addrspace(1)** %6, align 8 
  %8 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %4, i32  1, i8*  %5)  
  %9 = bitcast i8* %8 to i8 addrspace(1)** 
  %10 = load  i8 addrspace(1)*, i8 addrspace(1)** %9, align 8 
  ret i8* %8 
}


@$PrettyShow$Byte =    global {{i8*, i32, i32, i8*}} { {i8*, i32, i32, i8*} { i8* bitcast (i8* (i8*)* @$PrettyShow$Byte$ppShow to i8*), i32 1, i32 1, i8* undef } }


define external ccc  i8* @$Show$List$show(i8* )    {
  %2 = bitcast i8* (i8*, i8*)* @__7e60b3743245bd79b1de9c1b618cbcce__showWith to i8* 
  %3 = bitcast i8* (i8*)* @$Show$Byte$show to i8* 
  %4 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32, i8*}* getelementptr inbounds ({i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* inttoptr (i32 0 to {i8*, i32, i32, i8*}*), i32 1) to i64))  
  %5 = bitcast i8* %4 to {i8*, i32, i32, i8*}* 
  %6 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %5, i32 0, i32 0 
  store  i8* %3, i8** %6, align 8 
  %7 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %5, i32 0, i32 1 
  store  i32 1, i32* %7, align 8 
  %8 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %5, i32 0, i32 2 
  store  i32 1, i32* %8, align 8 
  %9 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*}* getelementptr inbounds ({i8*}, {i8*}* inttoptr (i32 0 to {i8*}*), i32 1) to i64))  
  %10 = bitcast i8* %9 to {i8*}* 
  %11 = getelementptr  {i8*}, {i8*}* %10, i32 0, i32 0 
  store  i8* %4, i8** %11, align 8 
  %12 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32, i8*}* getelementptr inbounds ({i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* inttoptr (i32 0 to {i8*, i32, i32, i8*}*), i32 1) to i64))  
  %13 = bitcast i8* %12 to {i8*, i32, i32, i8*}* 
  %14 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %13, i32 0, i32 0 
  store  i8* %2, i8** %14, align 8 
  %15 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %13, i32 0, i32 1 
  store  i32 2, i32* %15, align 8 
  %16 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %13, i32 0, i32 2 
  store  i32 1, i32* %16, align 8 
  %17 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %13, i32 0, i32 3 
  store  i8* %9, i8** %17, align 8 
  %18 = bitcast {i8*, i32, i32, i8*}* %13 to i8* 
  %19 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %18, i32  1, i8*  %0)  
  ret i8* %19 
}


@$Show$List =    global {{i8*, i32, i32, i8*}} { {i8*, i32, i32, i8*} { i8* bitcast (i8* (i8*)* @$Show$List$show to i8*), i32 1, i32 1, i8* undef } }


define external ccc  i8* @processItems$lifted$0(i8*  %__61ebaf0b80ceb01f7afef988602b6f1f__push_0, i8*  %with_0, i8*  %processed_0, i8*  %items_0)    {
; <label>:0:
  %1 = bitcast i8* %__61ebaf0b80ceb01f7afef988602b6f1f__push_0 to {i8*, i32, i32, i8*}* 
  %2 = bitcast i8* %with_0 to {i8*, i32, i32, i8*}* 
  %3 = bitcast i8* %processed_0 to i8 addrspace(1)** 
  %4 = load  i8 addrspace(1)*, i8 addrspace(1)** %3, align 8 
  %5 = bitcast i8* %items_0 to {i8*, i8*} addrspace(1)** 
  %6 = load  {i8*, i8*} addrspace(1)*, {i8*, i8*} addrspace(1)** %5, align 8 
  %7 =  call ccc  i1  @MadList_hasMinLength(double  2.000000e0, {i8*, i8*} addrspace(1)*  %6)  
  %8 = getelementptr  {i8*, i8*}, {i8*, i8*} addrspace(1)* %6, i32 0, i32 0 
  %9 = load  i8*, i8* addrspace(1)* %8, align 8 
  %10 = bitcast i8* %9 to i8* 
  %11 = getelementptr  {i8*, i8*}, {i8*, i8*} addrspace(1)* %6, i32 0, i32 1 
  %12 = load  i8*, i8* addrspace(1)* %11, align 8 
  %13 = addrspacecast i8* %12 to {i8*, i8*} addrspace(1)* 
  %14 = getelementptr  {i8*, i8*}, {i8*, i8*} addrspace(1)* %13, i32 0, i32 0 
  %15 = load  i8*, i8* addrspace(1)* %14, align 8 
  %16 = bitcast i8* %15 to i8* 
  %17 = getelementptr  {i8*, i8*}, {i8*, i8*} addrspace(1)* %13, i32 0, i32 1 
  %18 = load  i8*, i8* addrspace(1)* %17, align 8 
  %19 = addrspacecast i8* %18 to {i8*, i8*} addrspace(1)* 
  %20 = and i1 1, 1 
  %21 = and i1 1, %20 
  %22 = and i1 %7, %21 
  br i1 %22, label %branchExpBlock_0, label %nextBlock_0 
branchExpBlock_0:
  %23 = getelementptr  {i8*, i8*}, {i8*, i8*} addrspace(1)* %6, i32 0, i32 0 
  %24 = load  i8*, i8* addrspace(1)* %23, align 8 
  %25 = bitcast i8* %24 to i8* 
  %26 = getelementptr  {i8*, i8*}, {i8*, i8*} addrspace(1)* %6, i32 0, i32 1 
  %27 = load  i8*, i8* addrspace(1)* %26, align 8 
  %28 = addrspacecast i8* %27 to {i8*, i8*} addrspace(1)* 
  %29 = getelementptr  {i8*, i8*}, {i8*, i8*} addrspace(1)* %28, i32 0, i32 0 
  %30 = load  i8*, i8* addrspace(1)* %29, align 8 
  %31 = bitcast i8* %30 to i8* 
  %32 = getelementptr  {i8*, i8*}, {i8*, i8*} addrspace(1)* %28, i32 0, i32 1 
  %33 = load  i8*, i8* addrspace(1)* %32, align 8 
  %34 = addrspacecast i8* %33 to {i8*, i8*} addrspace(1)* 
  %35 = bitcast i8* (i8*)* @__61ebaf0b80ceb01f7afef988602b6f1f__push to i8* 
  %36 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32, i8*}* getelementptr inbounds ({i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* inttoptr (i32 0 to {i8*, i32, i32, i8*}*), i32 1) to i64))  
  %37 = bitcast i8* %36 to {i8*, i32, i32, i8*}* 
  %38 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %37, i32 0, i32 0 
  store  i8* %35, i8** %38, align 8 
  %39 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %37, i32 0, i32 1 
  store  i32 2, i32* %39, align 8 
  %40 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %37, i32 0, i32 2 
  store  i32 2, i32* %40, align 8 
  %41 = bitcast {i8*, i32, i32, i8*}* %2 to i8* 
  %42 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %41, i32  1, i8*  %25)  
  %43 = bitcast i8* %42 to i8 addrspace(1)** 
  %44 = load  i8 addrspace(1)*, i8 addrspace(1)** %43, align 8 
  %45 =  call ccc  i8*  @GC_malloc(i64  2)  
  %46 = addrspacecast i8* %45 to i8 addrspace(1)* 
  %47 = getelementptr  i8, i8 addrspace(1)* %46, i32 0 
  store  i8 32, i8 addrspace(1)* %47, align 8 
  %48 = getelementptr  i8, i8 addrspace(1)* %46, i32 1 
  store  i8 0, i8 addrspace(1)* %48, align 8 
  %49 =  call ccc  i8 addrspace(1)*  @__strConcat__(i8 addrspace(1)*  %44, i8 addrspace(1)*  %46)  
  %50 =  call ccc  i8 addrspace(1)*  @__strConcat__(i8 addrspace(1)*  %4, i8 addrspace(1)*  %49)  
  %51 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i8*} addrspace(1)** getelementptr inbounds ({i8*, i8*} addrspace(1)*, {i8*, i8*} addrspace(1)** inttoptr (i32 0 to {i8*, i8*} addrspace(1)**), i32 1) to i64))  
  %52 = bitcast i8* %51 to {i8*, i8*} addrspace(1)** 
  store  {i8*, i8*} addrspace(1)* %34, {i8*, i8*} addrspace(1)** %52, align 8 
  %53 = bitcast {i8*, i8*} addrspace(1)** %52 to i8* 
  %54 =  call ccc  i8*  @__61ebaf0b80ceb01f7afef988602b6f1f__push(i8*  %31, i8*  %53)  
  %55 = bitcast i8* %54 to {i8*, i8*} addrspace(1)** 
  %56 = load  {i8*, i8*} addrspace(1)*, {i8*, i8*} addrspace(1)** %55, align 8 
  %57 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (i8 addrspace(1)** getelementptr inbounds (i8 addrspace(1)*, i8 addrspace(1)** inttoptr (i32 0 to i8 addrspace(1)**), i32 1) to i64))  
  %58 = bitcast i8* %57 to i8 addrspace(1)** 
  store  i8 addrspace(1)* %50, i8 addrspace(1)** %58, align 8 
  %59 = bitcast i8 addrspace(1)** %58 to i8* 
  %60 =  call ccc  i8*  @processItems$lifted$0(i8*  %36, i8*  %with_0, i8*  %59, i8*  %54)  
  %61 = bitcast i8* %60 to i8 addrspace(1)** 
  %62 = load  i8 addrspace(1)*, i8 addrspace(1)** %61, align 8 
  br label %exitBlock_0 
nextBlock_0:
  %63 =  call ccc  i1  @MadList_hasLength(double  1.000000e0, {i8*, i8*} addrspace(1)*  %6)  
  %64 = getelementptr  {i8*, i8*}, {i8*, i8*} addrspace(1)* %6, i32 0, i32 0 
  %65 = load  i8*, i8** %64, align 8 
  %66 = bitcast i8* %65 to i8* 
  %67 = getelementptr  {i8*, i8*}, {i8*, i8*} addrspace(1)* %6, i32 0, i32 1 
  %68 = load  i8*, i8** %67, align 8 
  %69 = addrspacecast i8* %68 to {i8*, i8*} addrspace(1)* 
  %70 = and i1 1, 1 
  %71 = and i1 %63, %70 
  br i1 %71, label %branchExpBlock_1, label %nextBlock_1 
branchExpBlock_1:
  %72 = getelementptr  {i8*, i8*}, {i8*, i8*} addrspace(1)* %6, i32 0, i32 0 
  %73 = load  i8*, i8** %72, align 8 
  %74 = bitcast i8* %73 to i8* 
  %75 = getelementptr  {i8*, i8*}, {i8*, i8*} addrspace(1)* %6, i32 0, i32 1 
  %76 = load  i8*, i8** %75, align 8 
  %77 = addrspacecast i8* %76 to {i8*, i8*} addrspace(1)* 
  %78 = bitcast {i8*, i32, i32, i8*}* %2 to i8* 
  %79 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %78, i32  1, i8*  %74)  
  %80 = bitcast i8* %79 to i8 addrspace(1)** 
  %81 = load  i8 addrspace(1)*, i8 addrspace(1)** %80, align 8 
  %82 =  call ccc  i8 addrspace(1)*  @__strConcat__(i8 addrspace(1)*  %4, i8 addrspace(1)*  %81)  
  %83 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (i8 addrspace(1)** getelementptr inbounds (i8 addrspace(1)*, i8 addrspace(1)** inttoptr (i32 0 to i8 addrspace(1)**), i32 1) to i64))  
  %84 = bitcast i8* %83 to i8 addrspace(1)** 
  store  i8 addrspace(1)* %82, i8 addrspace(1)** %84, align 8 
  %85 = bitcast i8 addrspace(1)** %84 to i8* 
  br label %exitBlock_0 
nextBlock_1:
  %86 =  call ccc  i1  @MadList_hasLength(double  0.000000e0, {i8*, i8*} addrspace(1)*  %6)  
  %87 = and i1 %86, 1 
  br i1 %87, label %branchExpBlock_2, label %exitBlock_0 
branchExpBlock_2:
  %88 =  call ccc  i8*  @GC_malloc(i64  1)  
  %89 = addrspacecast i8* %88 to i8 addrspace(1)* 
  %90 = getelementptr  i8, i8 addrspace(1)* %89, i32 0 
  store  i8 0, i8* %90, align 8 
  %91 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (i8 addrspace(1)** getelementptr inbounds (i8 addrspace(1)*, i8 addrspace(1)** inttoptr (i32 0 to i8 addrspace(1)**), i32 1) to i64))  
  %92 = bitcast i8* %91 to i8 addrspace(1)** 
  store  i8 addrspace(1)* %89, i8 addrspace(1)** %92, align 8 
  %93 = bitcast i8 addrspace(1)** %92 to i8* 
  br label %exitBlock_0 
exitBlock_0:
  %94 = phi i8* [%60, %branchExpBlock_0], [%85, %branchExpBlock_1], [%93, %branchExpBlock_2], [zeroinitializer, %nextBlock_1] 
  ret i8* %94 
}


declare external ccc  i8 addrspace(1)* @__byteToStr__(i8)    


define external ccc  i8* @__7e60b3743245bd79b1de9c1b618cbcce__showByte(i8* )    {
  %2 = bitcast i8* %0 to i8* 
  %3 = load  i8, i8* %2, align 8 
  %4 =  call ccc  i8 addrspace(1)*  @__byteToStr__(i8  %3)  
  %5 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (i8 addrspace(1)** getelementptr inbounds (i8 addrspace(1)*, i8 addrspace(1)** inttoptr (i32 0 to i8 addrspace(1)**), i32 1) to i64))  
  %6 = bitcast i8* %5 to i8 addrspace(1)** 
  store  i8 addrspace(1)* %4, i8 addrspace(1)** %6, align 8 
  %7 = bitcast i8 addrspace(1)** %6 to i8* 
  ret i8* %7 
}


define external ccc  i8* @__7e60b3743245bd79b1de9c1b618cbcce__showWith(i8*  %with_0, i8*  %list_0)    {
  %1 = bitcast i8* %with_0 to {i8*, i32, i32, i8*}* 
  %2 = bitcast i8* %list_0 to {i8*, i8*} addrspace(1)** 
  %3 = load  {i8*, i8*} addrspace(1)*, {i8*, i8*} addrspace(1)** %2, align 8 
  %4 = bitcast i8* (i8*, i8*, i8*, i8*)* @processItems$lifted$0 to i8* 
  %5 = bitcast i8* (i8*)* @__61ebaf0b80ceb01f7afef988602b6f1f__push to i8* 
  %6 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32, i8*}* getelementptr inbounds ({i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* inttoptr (i32 0 to {i8*, i32, i32, i8*}*), i32 1) to i64))  
  %7 = bitcast i8* %6 to {i8*, i32, i32, i8*}* 
  %8 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %7, i32 0, i32 0 
  store  i8* %5, i8** %8, align 8 
  %9 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %7, i32 0, i32 1 
  store  i32 2, i32* %9, align 8 
  %10 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %7, i32 0, i32 2 
  store  i32 2, i32* %10, align 8 
  %11 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i8*}* getelementptr inbounds ({i8*, i8*}, {i8*, i8*}* inttoptr (i32 0 to {i8*, i8*}*), i32 1) to i64))  
  %12 = bitcast i8* %11 to {i8*, i8*}* 
  %13 = getelementptr  {i8*, i8*}, {i8*, i8*}* %12, i32 0, i32 0 
  store  i8* %6, i8** %13, align 8 
  %14 = getelementptr  {i8*, i8*}, {i8*, i8*}* %12, i32 0, i32 1 
  store  i8* %with_0, i8** %14, align 8 
  %15 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32, i8*}* getelementptr inbounds ({i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* inttoptr (i32 0 to {i8*, i32, i32, i8*}*), i32 1) to i64))  
  %16 = bitcast i8* %15 to {i8*, i32, i32, i8*}* 
  %17 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %16, i32 0, i32 0 
  store  i8* %4, i8** %17, align 8 
  %18 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %16, i32 0, i32 1 
  store  i32 4, i32* %18, align 8 
  %19 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %16, i32 0, i32 2 
  store  i32 2, i32* %19, align 8 
  %20 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %16, i32 0, i32 3 
  store  i8* %11, i8** %20, align 8 
  %21 = bitcast {i8*, i32, i32, i8*}* %16 to i8* 
  %22 =  call ccc  i8*  @GC_malloc(i64  1)  
  %23 = addrspacecast i8* %22 to i8 addrspace(1)* 
  %24 = getelementptr  i8, i8 addrspace(1)* %23, i32 0 
  store  i8 0, i8* %24, align 8 
  %25 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (i8 addrspace(1)** getelementptr inbounds (i8 addrspace(1)*, i8 addrspace(1)** inttoptr (i32 0 to i8 addrspace(1)**), i32 1) to i64))  
  %26 = bitcast i8* %25 to i8 addrspace(1)** 
  store  i8 addrspace(1)* %23, i8 addrspace(1)** %26, align 8 
  %27 = bitcast i8 addrspace(1)** %26 to i8* 
  %28 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %21, i32  2, i8*  %27, i8*  %list_0)  
  %29 = bitcast i8* %28 to i8 addrspace(1)** 
  %30 = load  i8 addrspace(1)*, i8 addrspace(1)** %29, align 8 
  ret i8* %28 
}


declare external ccc  i8* @__applyPAP__(i8*, i32, ...)    


declare external ccc  {i32, i8*}* @__buildRecord__(i32, i8*, ...)    


declare external ccc  i8* @__selectField__(i8 addrspace(1)*, {i32, i8*}*)    


declare external ccc  i1 @__streq__(i8 addrspace(1)*, i8 addrspace(1)*)    


declare external ccc  i8 addrspace(1)* @__strConcat__(i8 addrspace(1)*, i8 addrspace(1)*)    


declare external ccc  i1 @MadList_hasMinLength(double, {i8*, i8*} addrspace(1)*)    


declare external ccc  i1 @MadList_hasLength(double, {i8*, i8*} addrspace(1)*)    


declare external ccc  {i8*, i8*} addrspace(1)* @MadList_singleton(i8*)    


declare external ccc  {i8*, i8*} addrspace(1)* @__MadList_push__(i8*, {i8*, i8*} addrspace(1)*)    


declare external ccc  {i8*, i8*} addrspace(1)* @MadList_concat({i8*, i8*} addrspace(1)*, {i8*, i8*} addrspace(1)*)    


declare external ccc  i8* @GC_malloc(i64)    


declare external ccc  i8* @malloc(i64)    


declare external ccc  i8* @calloc(i32, i32)    


declare external ccc  void @__initEventLoop__()    


declare external ccc  void @__startEventLoop__()    


declare external ccc  void @__0ca498e5e1cdf5d72e5690ca02bcce97__moduleFunction()    


declare external ccc  void @__5dd8d8f67f6caa3e4c41a3cdcb44ab61__moduleFunction()    


declare external ccc  void @__61ebaf0b80ceb01f7afef988602b6f1f__moduleFunction()    


declare external ccc  void @__b4db05d793a8756e66d267415704250c__moduleFunction()    


declare external ccc  void @__c700acae529ff947acae37ffd524bdc6__moduleFunction()    


declare external ccc  void @__c74b94e1f65bda9e07e07469839aae9a__moduleFunction()    


declare external ccc  void @__cebca4c98d7d0c8157e1ed2c6179f816__moduleFunction()    


declare external ccc  void @__e3e8c77e8e733398f092072b275bef7e__moduleFunction()    


declare external ccc  void @__f5f170f8b96f1ba835b8d043e370378e__moduleFunction()    


define external ccc  void @main()    {
entry_0:
   call ccc  void  @__initEventLoop__()  
   call ccc  void  @__0ca498e5e1cdf5d72e5690ca02bcce97__moduleFunction()  
   call ccc  void  @__5dd8d8f67f6caa3e4c41a3cdcb44ab61__moduleFunction()  
   call ccc  void  @__61ebaf0b80ceb01f7afef988602b6f1f__moduleFunction()  
   call ccc  void  @__b4db05d793a8756e66d267415704250c__moduleFunction()  
   call ccc  void  @__c700acae529ff947acae37ffd524bdc6__moduleFunction()  
   call ccc  void  @__c74b94e1f65bda9e07e07469839aae9a__moduleFunction()  
   call ccc  void  @__cebca4c98d7d0c8157e1ed2c6179f816__moduleFunction()  
   call ccc  void  @__e3e8c77e8e733398f092072b275bef7e__moduleFunction()  
   call ccc  void  @__f5f170f8b96f1ba835b8d043e370378e__moduleFunction()  
   call ccc  void  @__startEventLoop__()  
  ret void 
}