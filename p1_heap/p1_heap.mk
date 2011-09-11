##
## Auto Generated makefile by CodeLite IDE
## any manual changes will be erased      
##
## Release
ProjectName            :=p1_heap
ConfigurationName      :=Release
IntermediateDirectory  :=./Release
OutDir                 := $(IntermediateDirectory)
WorkspacePath          := "/home/hvidgaard/Desktop/Dropbox/2011q1+2 AADS"
ProjectPath            := "/home/hvidgaard/Desktop/Dropbox/2011q1+2 AADS/p1_heap"
CurrentFileName        :=
CurrentFilePath        :=
CurrentFileFullPath    :=
User                   :=Peter Hvidgaard
Date                   :=09/11/2011
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
CmpOptions             :=  $(Preprocessors)
LinkOptions            :=  -O2
IncludePath            :=  "$(IncludeSwitch)." "$(IncludeSwitch)." 
RcIncludePath          :=
Libs                   :=
LibPath                := "$(LibraryPathSwitch)." 


##
## User defined environment variables
##
CodeLiteDir:=/usr/share/codelite
Objects=$(IntermediateDirectory)/main$(ObjectSuffix) $(IntermediateDirectory)/BinaryHeap$(ObjectSuffix) $(IntermediateDirectory)/FibonacciHeap$(ObjectSuffix) 

##
## Main Build Targets 
##
all: $(OutputFile)

$(OutputFile): makeDirStep $(Objects)
	@$(MakeDirCommand) $(@D)
	$(LinkerName) $(OutputSwitch)$(OutputFile) $(Objects) $(LibPath) $(Libs) $(LinkOptions)

makeDirStep:
	@test -d ./Release || $(MakeDirCommand) ./Release

PreBuild:


##
## Objects
##
$(IntermediateDirectory)/main$(ObjectSuffix): main.c $(IntermediateDirectory)/main$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "/home/hvidgaard/Desktop/Dropbox/2011q1+2 AADS/p1_heap/main.c" $(CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/main$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/main$(DependSuffix): main.c
	@$(C_CompilerName) $(CmpOptions) $(IncludePath) -MT$(IntermediateDirectory)/main$(ObjectSuffix) -MF$(IntermediateDirectory)/main$(DependSuffix) -MM "/home/hvidgaard/Desktop/Dropbox/2011q1+2 AADS/p1_heap/main.c"

$(IntermediateDirectory)/main$(PreprocessSuffix): main.c
	@$(C_CompilerName) $(CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/main$(PreprocessSuffix) "/home/hvidgaard/Desktop/Dropbox/2011q1+2 AADS/p1_heap/main.c"

$(IntermediateDirectory)/BinaryHeap$(ObjectSuffix): BinaryHeap.c $(IntermediateDirectory)/BinaryHeap$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "/home/hvidgaard/Desktop/Dropbox/2011q1+2 AADS/p1_heap/BinaryHeap.c" $(CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/BinaryHeap$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/BinaryHeap$(DependSuffix): BinaryHeap.c
	@$(C_CompilerName) $(CmpOptions) $(IncludePath) -MT$(IntermediateDirectory)/BinaryHeap$(ObjectSuffix) -MF$(IntermediateDirectory)/BinaryHeap$(DependSuffix) -MM "/home/hvidgaard/Desktop/Dropbox/2011q1+2 AADS/p1_heap/BinaryHeap.c"

$(IntermediateDirectory)/BinaryHeap$(PreprocessSuffix): BinaryHeap.c
	@$(C_CompilerName) $(CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/BinaryHeap$(PreprocessSuffix) "/home/hvidgaard/Desktop/Dropbox/2011q1+2 AADS/p1_heap/BinaryHeap.c"

$(IntermediateDirectory)/FibonacciHeap$(ObjectSuffix): FibonacciHeap.c $(IntermediateDirectory)/FibonacciHeap$(DependSuffix)
	$(C_CompilerName) $(SourceSwitch) "/home/hvidgaard/Desktop/Dropbox/2011q1+2 AADS/p1_heap/FibonacciHeap.c" $(CmpOptions) $(ObjectSwitch)$(IntermediateDirectory)/FibonacciHeap$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/FibonacciHeap$(DependSuffix): FibonacciHeap.c
	@$(C_CompilerName) $(CmpOptions) $(IncludePath) -MT$(IntermediateDirectory)/FibonacciHeap$(ObjectSuffix) -MF$(IntermediateDirectory)/FibonacciHeap$(DependSuffix) -MM "/home/hvidgaard/Desktop/Dropbox/2011q1+2 AADS/p1_heap/FibonacciHeap.c"

$(IntermediateDirectory)/FibonacciHeap$(PreprocessSuffix): FibonacciHeap.c
	@$(C_CompilerName) $(CmpOptions) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/FibonacciHeap$(PreprocessSuffix) "/home/hvidgaard/Desktop/Dropbox/2011q1+2 AADS/p1_heap/FibonacciHeap.c"


-include $(IntermediateDirectory)/*$(DependSuffix)
##
## Clean
##
clean:
	$(RM) $(IntermediateDirectory)/main$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/main$(DependSuffix)
	$(RM) $(IntermediateDirectory)/main$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/BinaryHeap$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/BinaryHeap$(DependSuffix)
	$(RM) $(IntermediateDirectory)/BinaryHeap$(PreprocessSuffix)
	$(RM) $(IntermediateDirectory)/FibonacciHeap$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/FibonacciHeap$(DependSuffix)
	$(RM) $(IntermediateDirectory)/FibonacciHeap$(PreprocessSuffix)
	$(RM) $(OutputFile)


