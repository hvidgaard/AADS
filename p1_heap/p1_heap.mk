##
## Auto Generated makefile by CodeLite IDE
## any manual changes will be erased      
##
## Debug
ProjectName            :=p1_heap
ConfigurationName      :=Debug
IntermediateDirectory  :=./Debug
OutDir                 := $(IntermediateDirectory)
WorkspacePath          := "/home/hvidgaard/Desktop/Dropbox/2011q1+2 AADS"
ProjectPath            := "/home/hvidgaard/Desktop/Dropbox/2011q1+2 AADS/p1_heap"
CurrentFileName        :=
CurrentFilePath        :=
CurrentFileFullPath    :=
User                   :=Peter Hvidgaard
Date                   :=09/20/2011
CodeLitePath           :="/home/hvidgaard/.codelite"
LinkerName             :=gcc
ArchiveTool            :=ar rcus
SharedObjectLinkerName :=gcc -shared -fPIC
ObjectSuffix           :=.o
DependSuffix           :=.o.d
PreprocessSuffix       :=.o.i
DebugSwitch            :=-g 
IncludeSwitch          :=-I
LibrarySwitch          :=-l
OutputSwitch           :=-o 
LibraryPathSwitch      :=-L
PreprocessorSwitch     :=-D
SourceSwitch           :=-c 
CompilerName           :=gcc
C_CompilerName         :=gcc
OutputFile             :=$(IntermediateDirectory)/$(ProjectName)
Preprocessors          :=
ObjectSwitch           :=-o 
ArchiveOutputSwitch    := 
PreprocessOnlySwitch   :=-E 
MakeDirCommand         :=mkdir -p
CmpOptions             := -g $(Preprocessors)
LinkOptions            :=  
IncludePath            :=  "$(IncludeSwitch)." "$(IncludeSwitch)." "$(IncludeSwitch)./src" 
RcIncludePath          :=
Libs                   :=
LibPath                := "$(LibraryPathSwitch)." 


##
## User defined environment variables
##
CodeLiteDir:=/usr/share/codelite
Objects=$(IntermediateDirectory)/src_AbstractHeap$(ObjectSuffix) $(IntermediateDirectory)/src_BinaryHeap$(ObjectSuffix) $(IntermediateDirectory)/src_DijkstraSSSP$(ObjectSuffix) $(IntermediateDirectory)/src_FibonacciHeap$(ObjectSuffix) $(IntermediateDirectory)/src_Testing$(ObjectSuffix) 

##
## Main Build Targets 
##
all: $(OutputFile)

$(OutputFile): makeDirStep $(Objects)
	@$(MakeDirCommand) $(@D)
	$(LinkerName) $(OutputSwitch)$(OutputFile) $(Objects) $(LibPath) $(Libs) $(LinkOptions)

makeDirStep:
	@test -d ./Debug || $(MakeDirCommand) ./Debug

PreBuild:


##
## Objects
##
$(IntermediateDirectory)/src_AbstractHeap$(ObjectSuffix): src/AbstractHeap.c $(IntermediateDirectory)/src_AbstractHeap$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "/home/hvidgaard/Desktop/Dropbox/2011q1+2 AADS/p1_heap/src/AbstractHeap.c" $(CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/src_AbstractHeap$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/src_AbstractHeap$(DependSuffix): src/AbstractHeap.c
	@$(C_CompilerName) $(CmpOptions) $(IncludePath) -MT$(IntermediateDirectory)/src_AbstractHeap$(ObjectSuffix) -MF$(IntermediateDirectory)/src_AbstractHeap$(DependSuffix) -MM "/home/hvidgaard/Desktop/Dropbox/2011q1+2 AADS/p1_heap/src/AbstractHeap.c"

$(IntermediateDirectory)/src_AbstractHeap$(PreprocessSuffix): src/AbstractHeap.c
	@$(C_CompilerName) $(CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/src_AbstractHeap$(PreprocessSuffix) "/home/hvidgaard/Desktop/Dropbox/2011q1+2 AADS/p1_heap/src/AbstractHeap.c"

$(IntermediateDirectory)/src_BinaryHeap$(ObjectSuffix): src/BinaryHeap.c $(IntermediateDirectory)/src_BinaryHeap$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "/home/hvidgaard/Desktop/Dropbox/2011q1+2 AADS/p1_heap/src/BinaryHeap.c" $(CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/src_BinaryHeap$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/src_BinaryHeap$(DependSuffix): src/BinaryHeap.c
	@$(C_CompilerName) $(CmpOptions) $(IncludePath) -MT$(IntermediateDirectory)/src_BinaryHeap$(ObjectSuffix) -MF$(IntermediateDirectory)/src_BinaryHeap$(DependSuffix) -MM "/home/hvidgaard/Desktop/Dropbox/2011q1+2 AADS/p1_heap/src/BinaryHeap.c"

$(IntermediateDirectory)/src_BinaryHeap$(PreprocessSuffix): src/BinaryHeap.c
	@$(C_CompilerName) $(CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/src_BinaryHeap$(PreprocessSuffix) "/home/hvidgaard/Desktop/Dropbox/2011q1+2 AADS/p1_heap/src/BinaryHeap.c"

$(IntermediateDirectory)/src_DijkstraSSSP$(ObjectSuffix): src/DijkstraSSSP.c $(IntermediateDirectory)/src_DijkstraSSSP$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "/home/hvidgaard/Desktop/Dropbox/2011q1+2 AADS/p1_heap/src/DijkstraSSSP.c" $(CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/src_DijkstraSSSP$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/src_DijkstraSSSP$(DependSuffix): src/DijkstraSSSP.c
	@$(C_CompilerName) $(CmpOptions) $(IncludePath) -MT$(IntermediateDirectory)/src_DijkstraSSSP$(ObjectSuffix) -MF$(IntermediateDirectory)/src_DijkstraSSSP$(DependSuffix) -MM "/home/hvidgaard/Desktop/Dropbox/2011q1+2 AADS/p1_heap/src/DijkstraSSSP.c"

$(IntermediateDirectory)/src_DijkstraSSSP$(PreprocessSuffix): src/DijkstraSSSP.c
	@$(C_CompilerName) $(CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/src_DijkstraSSSP$(PreprocessSuffix) "/home/hvidgaard/Desktop/Dropbox/2011q1+2 AADS/p1_heap/src/DijkstraSSSP.c"

$(IntermediateDirectory)/src_FibonacciHeap$(ObjectSuffix): src/FibonacciHeap.c $(IntermediateDirectory)/src_FibonacciHeap$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "/home/hvidgaard/Desktop/Dropbox/2011q1+2 AADS/p1_heap/src/FibonacciHeap.c" $(CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/src_FibonacciHeap$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/src_FibonacciHeap$(DependSuffix): src/FibonacciHeap.c
	@$(C_CompilerName) $(CmpOptions) $(IncludePath) -MT$(IntermediateDirectory)/src_FibonacciHeap$(ObjectSuffix) -MF$(IntermediateDirectory)/src_FibonacciHeap$(DependSuffix) -MM "/home/hvidgaard/Desktop/Dropbox/2011q1+2 AADS/p1_heap/src/FibonacciHeap.c"

$(IntermediateDirectory)/src_FibonacciHeap$(PreprocessSuffix): src/FibonacciHeap.c
	@$(C_CompilerName) $(CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/src_FibonacciHeap$(PreprocessSuffix) "/home/hvidgaard/Desktop/Dropbox/2011q1+2 AADS/p1_heap/src/FibonacciHeap.c"

$(IntermediateDirectory)/src_Testing$(ObjectSuffix): src/Testing.c $(IntermediateDirectory)/src_Testing$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "/home/hvidgaard/Desktop/Dropbox/2011q1+2 AADS/p1_heap/src/Testing.c" $(CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/src_Testing$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/src_Testing$(DependSuffix): src/Testing.c
	@$(C_CompilerName) $(CmpOptions) $(IncludePath) -MT$(IntermediateDirectory)/src_Testing$(ObjectSuffix) -MF$(IntermediateDirectory)/src_Testing$(DependSuffix) -MM "/home/hvidgaard/Desktop/Dropbox/2011q1+2 AADS/p1_heap/src/Testing.c"

$(IntermediateDirectory)/src_Testing$(PreprocessSuffix): src/Testing.c
	@$(C_CompilerName) $(CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/src_Testing$(PreprocessSuffix) "/home/hvidgaard/Desktop/Dropbox/2011q1+2 AADS/p1_heap/src/Testing.c"


-include $(IntermediateDirectory)/*$(DependSuffix)
##
## Clean
##
clean:
	$(RM) $(IntermediateDirectory)/src_AbstractHeap$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/src_AbstractHeap$(DependSuffix)
	$(RM) $(IntermediateDirectory)/src_AbstractHeap$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/src_BinaryHeap$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/src_BinaryHeap$(DependSuffix)
	$(RM) $(IntermediateDirectory)/src_BinaryHeap$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/src_DijkstraSSSP$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/src_DijkstraSSSP$(DependSuffix)
	$(RM) $(IntermediateDirectory)/src_DijkstraSSSP$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/src_FibonacciHeap$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/src_FibonacciHeap$(DependSuffix)
	$(RM) $(IntermediateDirectory)/src_FibonacciHeap$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/src_Testing$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/src_Testing$(DependSuffix)
	$(RM) $(IntermediateDirectory)/src_Testing$(PreprocessSuffix)
	$(RM) $(OutputFile)


