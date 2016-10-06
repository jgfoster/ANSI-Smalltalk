"Filed out from Dolphin Smalltalk 7"!

Object subclass: #AnsiComponent
	instanceVariableNames: 'stream section sectionNumber name'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
AnsiComponent guid: (GUID fromString: '{756D2F40-4863-4ECA-8DCC-602A0DB66D85}')!
AnsiComponent comment: ''!
!AnsiComponent categoriesForClass!Unclassified! !
!AnsiComponent methodsFor!

<= anAnsiComponent

	^self sectionNumber <= anAnsiComponent sectionNumber.
!

name

	^name.
!

printOn: aStream

	aStream
		nextPutAll: section; space;
		nextPutAll: name;
		yourself.
!

section
	^section!

sectionNumber

	sectionNumber isNil ifTrue: [
		sectionNumber := 0.
		(section subStrings: $.) do: [:each | 
			sectionNumber := sectionNumber * 100 + each asNumber.
		].
	].
	^sectionNumber.
! !
!AnsiComponent categoriesFor: #<=!public! !
!AnsiComponent categoriesFor: #name!public! !
!AnsiComponent categoriesFor: #printOn:!public! !
!AnsiComponent categoriesFor: #section!accessing!public! !
!AnsiComponent categoriesFor: #sectionNumber!public! !

