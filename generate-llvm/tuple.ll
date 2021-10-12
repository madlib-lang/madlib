; ModuleID = 'generate-llvm/tuple.cpp'
source_filename = "generate-llvm/tuple.cpp"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

%struct.Tuple2 = type { i8*, i8* }

; Function Attrs: noinline optnone ssp uwtable
define i8* @_Z1vv() #0 {
  %1 = alloca i8*, align 8
  %2 = call i8* @malloc(i64 80) #5
  store i8* %2, i8** %1, align 8
  %3 = load i8*, i8** %1, align 8
  ret i8* %3
}

; Function Attrs: allocsize(0)
declare i8* @malloc(i64) #1

; Function Attrs: noinline optnone ssp uwtable
define %struct.Tuple2* @createTuple2(i8*, i8*) #0 {
  %3 = alloca i8*, align 8
  %4 = alloca i8*, align 8
  %5 = alloca %struct.Tuple2*, align 8
  store i8* %0, i8** %3, align 8
  store i8* %1, i8** %4, align 8
  %6 = call i8* @_Znwm(i64 16) #6
  %7 = bitcast i8* %6 to %struct.Tuple2*
  %8 = bitcast %struct.Tuple2* %7 to i8*
  call void @llvm.memset.p0i8.i64(i8* align 16 %8, i8 0, i64 16, i1 false)
  store %struct.Tuple2* %7, %struct.Tuple2** %5, align 8
  %9 = load i8*, i8** %3, align 8
  %10 = load %struct.Tuple2*, %struct.Tuple2** %5, align 8
  %11 = getelementptr inbounds %struct.Tuple2, %struct.Tuple2* %10, i32 0, i32 0
  store i8* %9, i8** %11, align 8
  %12 = load i8*, i8** %4, align 8
  %13 = load %struct.Tuple2*, %struct.Tuple2** %5, align 8
  %14 = getelementptr inbounds %struct.Tuple2, %struct.Tuple2* %13, i32 0, i32 1
  store i8* %12, i8** %14, align 8
  %15 = load %struct.Tuple2*, %struct.Tuple2** %5, align 8
  ret %struct.Tuple2* %15
}

; Function Attrs: nobuiltin
declare noalias i8* @_Znwm(i64) #2

; Function Attrs: argmemonly nounwind
declare void @llvm.memset.p0i8.i64(i8* nocapture writeonly, i8, i64, i1 immarg) #3

; Function Attrs: noinline nounwind optnone ssp uwtable
define i8* @fst(%struct.Tuple2*) #4 {
  %2 = alloca %struct.Tuple2*, align 8
  store %struct.Tuple2* %0, %struct.Tuple2** %2, align 8
  %3 = load %struct.Tuple2*, %struct.Tuple2** %2, align 8
  %4 = getelementptr inbounds %struct.Tuple2, %struct.Tuple2* %3, i32 0, i32 0
  %5 = load i8*, i8** %4, align 8
  ret i8* %5
}

; Function Attrs: noinline nounwind optnone ssp uwtable
define i8* @snd(%struct.Tuple2*) #4 {
  %2 = alloca %struct.Tuple2*, align 8
  store %struct.Tuple2* %0, %struct.Tuple2** %2, align 8
  %3 = load %struct.Tuple2*, %struct.Tuple2** %2, align 8
  %4 = getelementptr inbounds %struct.Tuple2, %struct.Tuple2* %3, i32 0, i32 1
  %5 = load i8*, i8** %4, align 8
  ret i8* %5
}

attributes #0 = { noinline optnone ssp uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "darwin-stkchk-strong-link" "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "probe-stack"="___chkstk_darwin" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+cx8,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { allocsize(0) "correctly-rounded-divide-sqrt-fp-math"="false" "darwin-stkchk-strong-link" "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "probe-stack"="___chkstk_darwin" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+cx8,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = { nobuiltin "correctly-rounded-divide-sqrt-fp-math"="false" "darwin-stkchk-strong-link" "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "probe-stack"="___chkstk_darwin" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+cx8,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #3 = { argmemonly nounwind }
attributes #4 = { noinline nounwind optnone ssp uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "darwin-stkchk-strong-link" "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "probe-stack"="___chkstk_darwin" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+cx8,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #5 = { allocsize(0) }
attributes #6 = { builtin }

!llvm.module.flags = !{!0, !1, !2}
!llvm.ident = !{!3}

!0 = !{i32 2, !"SDK Version", [3 x i32] [i32 10, i32 15, i32 4]}
!1 = !{i32 1, !"wchar_size", i32 4}
!2 = !{i32 7, !"PIC Level", i32 2}
!3 = !{!"Apple clang version 11.0.3 (clang-1103.0.32.62)"}
