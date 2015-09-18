      PROGRAM GEIGER
C
C HELENA HUANG
C NOVEMBER 18, 2013
C
C Geiger Counter Data
C
C PRELIMINARY
C Processes a text file (GeigerData.txt) containing dates and
C corresponding Geiger data. Stores dates in one array (DTEARR)
C and the Geiger values in another (VALARR).
C To test the arrays, it prints the first and last three dates and
C values to be compared to the file.
C
C SECOND PASS :o)
C The subroutine ARRSUB now includes all the subroutines and functions
C which need arrays. The program now finds the minimum, maximum, range,
C mean, and standard deviation of the Geiger values, all of which are saved
C in a common block. It creates a histogram of the Geiger values using
C subroutine GRAPH, then closes the file and prints the common block
C variables mentioned above.
C 

C
C PARAMETERS
C 
C UNITN - The unit number (which is an arbitrary number greater than
C ten) which I will assign to the text file.
C
      INTEGER*4 UNITN
      PARAMETER (UNITN = 15)

C
C LOCAL VARIABLES
C 
C NLINES - The number of lines in the text file.
C
      INTEGER*4 NLINES

C
C FUNCTION DECLARATIONS
C
C LINECT - Returns the number of lines in the text file.
C FINDMN - Returns the mean of the Geiger values.
C FINDST - Returns the standard deviation of the Geiger values.
C FINDMD - Returns the mode of the Geiger values.
C
      INTEGER*4 LINECT
      REAL*4 FINDMN
      REAL*4 FINDST
      INTEGER*4 FINDMD

C
C COMMON VARIABLES
C
C MINN - The minimum value of the Geiger values.
C MAXX - The maximum value of the Geiger values.
C RANGE - The range of the Geiger values.
C MEAN - The mean value of the Geiger values.
C STDV - The standard deviation of the Geiger values.
C
      INTEGER*4 MINN
      INTEGER*4 MAXX
      INTEGER*4 RANGE
      REAL*4 MEAN
      REAL*4 STDV
      COMMON /STATS/ MINN, MAXX, RANGE, MEAN, STDV

C
C DATA STATEMENTS
C

C
C MAIN PROGRAM MODULE
C 
C Opens the file GeigerData.txt and sets its unit number to UNITN.
C Sets NLINES to the number of lines (found by LINECT), then rewinds
C to the beginning of the file and calls subroutine ARRSUB, which
C contains all the subroutines and functions which use arrays.
C
      OPEN(UNIT=UNITN, FILE='GeigerData.txt', STATUS='OLD')
      NLINES=LINECT(UNITN)
      REWIND(UNITN)
      CALL ARRSUB(UNITN, NLINES)

      CLOSE(UNITN)

      WRITE(*,*) "Min.:  ", MINN
      WRITE(*,*) "Max.:  ", MAXX
      WRITE(*,*) "Range: ", RANGE
      WRITE(*,*) "Mean:  ", MEAN
      WRITE(*,*) "Stdv.: ", STDV

      STOP
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SUBROUTINES
C
C
C ARRSUB
C     DESCRIPTION: 
C     Calls all the subroutines and functions which create/need the arrays
C     containing the dates and the Geiger values, simply because having
C     them all within the subroutine which created the arrays bothered me.
C     Creates the data arrays using the subroutines DATARR, then sets
C     the main program's MINN, MAXX, MEAN, and STDV using the subroutine
C     MINMAX and the functions FINDMN and FINDST. Calls subroutine SETFRQ
C     to set the array of the frequencies of the Geiger values, and
C     subroutine GRAPH to create a histograph of the Geiger values.
C     
C     ARGUMENTS:
C     UNITN - The unit number assigned to the text file.
C     NLINES - The number of lines in the text file.
C     MINN - The minimum value of the Geiger values.
C     MAXX - The maximum value of the Geiger values.
C     RANGE - The rage of the Geiger values.
C     MEAN - The mean value of the Geiger values.
C     STDV - The standard deviation of the Geiger values.
C     
C     PRECONDITION:
C     NLINES has been set to equal the number of lines. The data
C     is at the beginning of the file.
C
C
      SUBROUTINE ARRSUB(UNITN, NLINES)
c     Arguments
      INTEGER*4 UNITN
      INTEGER*4 NLINES

C     Local variables
      INTEGER*8 DTEARR(NLINES)
      INTEGER*4 VALARR(NLINES)
      INTEGER*4 ISTARR(3)
      REAL*4 RSTARR(2)

C     Functions
      REAL*4 FINDMN
      REAL*4 FINDST

C     Common variables
      INTEGER*4 MINN
      INTEGER*4 MAXX
      INTEGER*4 RANGE
      REAL*4 MEAN
      REAL*4 STDV
      COMMON /STATS/ MINN, MAXX, RANGE, MEAN, STDV

      EQUIVALENCE(MINN, ISTARR(1))
      EQUIVALENCE(MEAN, RSTARR(1))
      ISTARR=0
      RSTARR=0

      CALL DATARR(UNITN, NLINES, DTEARR, VALARR)
      CALL MINMAX(NLINES, VALARR, MINN, MAXX, RANGE)
      MEAN = FINDMN(NLINES, VALARR)
      STDV = FINDST(NLINES, VALARR, MEAN)

      CALL GRAPH(MINN, RANGE, NLINES, VALARR)

      RETURN
      END

C
C DATARR
C     DESCRIPTION: 
C     Creates two arrays (DTEARR and VALARR) containing,
C     respectively, the dates of the data and the Geiger
C     values (which are the two columns in the data file).
C     
C     ARGUMENTS:
C     UNITN - The unit number assigned to the text file.
C     NLINES - The number of lines in the text file.
C     DTEARR - The array of dates.
C     VALARR - The array of Geiger values.
C     
C     PRECONDITION:
C     NLINES has been set to equal the number of lines. The data
C     is at the beginning of the file.
C     POSTCONDITION:
C     Arrays DTEARR and VALARR of size NLINES have been filled. The first
C     and last three of each have been printed for testing purposes.
C
C
      SUBROUTINE DATARR(UNITN, NLINES, DTEARR, VALARR)
      INTEGER*4 UNITN
      INTEGER*4 NLINES
      INTEGER*8 DTEARR(NLINES)
      INTEGER*4 VALARR(NLINES)

      INTEGER*4 I

      DTEARR=0
      VALARR=0

      DO 120 I=1, NLINES
          READ(UNITN, 1000) DTEARR(I), VALARR(I)
1000      FORMAT(I10, 1X, I2)
120   CONTINUE
      
CCCC  Printing the first and last three dates and values

      WRITE(*,*) 'First three dates and corresponding values:'
      DO 130 I=1, 3
         WRITE(*,2000) DTEARR(I), VALARR(I)
130   CONTINUE
      WRITE(*,*) 'Last three dates and corresponding values:'
      DO 140 I=NLINES-2, NLINES
         WRITE(*,2000) DTEARR(I), VALARR(I)
140   CONTINUE

      RETURN
2000  FORMAT(3x, I10, 1x, I2)
      END


C
C MINMAX
C     DESCRIPTION: 
C     Sets the main program's local variables MINN and MAXX by
C     by traversing VALARR and checking whether each value is either
C     less than the current MINN or greater than the current MAXX.
C     Also sets RANGE to MAXX-MINN.
C     
C     ARGUMENTS:
C     UNITN - The unit number assigned to the text file.
C     NLINES - The number of lines in the text file.
C     VALARR - The array containing all the Geiger values.
C     MINN - The minimum Geiger value.
C     MAXX - The maximum Geiger value.
C     RANGE - The range of Geiger values.
C     
C     PRECONDITION:
C     The args have already been set and are defined in the module calling
C     MINMAX.
C     POSTCONDITION:
C     Main program's MINN and MAXX have been set to the Geiger value
C     minimum and maximum, respectively.
C
C
      SUBROUTINE MINMAX(NLINES, VALARR, MINN, MAXX, RANGE)
      INTEGER*4 NLINES
      INTEGER*4 vALARR(NLINES)
      INTEGER*4 MINN
      INTEGER*4 MAXX
      INTEGER*4 RANGE

      INTEGER*4 I

      MINN = VALARR(1)
      MAXX = VALARR(1)
      DO 150 I=2, NLINES
         IF(VALARR(I)<MINN) MINN=VALARR(I)
         IF(VALARR(I)>MAXX) MAXX=VALARR(I)
150   CONTINUE
      RANGE=MAXX-MINN+1

      RETURN
      END

C
C SETFRQ
C     DESCRIPTION: 
C     Creates an array of size RANGE which contains the frequency of
C     every Geiger value within the range, so the first index of the array
C     represents the frequency of the minimum value, and the last index
C     represents the frequency of the maximum value. The index of each
C     frequency is one greater than the Geiger value it represents, because
C     the smallest Geiger value possible is 0, so to prevent trying to access
C     FRQARR(0), 1 is added to the indices.
C     
C     ARGUMENTS:
C     NLINES - The number of lines in the text file.
C     VALARR - The array containing all the Geiger values.
C     RANGE - The range of the Gieger values.
C     FRQARR - The array of frequencies which this subroutine is setting.
C     
C     PRECONDITION:
C     The args have already been set and are defined in the module calling
C     SETFRQ.
C     POSTCONDITION:
C     The array FRQARR in the subroutine ARRSUB has been set.
C
C
      SUBROUTINE SETFRQ(NLINES, VALARR, RANGE, FRQARR)
      INTEGER*4 NLINES
      INTEGER*4 vALARR(NLINES)
      INTEGER*4 RANGE
      INTEGER*4 FRQARR(RANGE)

      INTEGER*4 I

      FRQARR=0

      DO 180 I=1, NLINES
         FRQARR(VALARR(I)+1)=FRQARR(VALARR(I)+1)+1
180   CONTINUE

      RETURN
      END

C
C GRAPH
C     DESCRIPTION: 
C     Creates a histogram of the Geiger data by obtaining the maximum height
C     the user wants (MAXHT), then calling another subroutine DRAW with this
C     value passed as an argument.
C     
C     ARGUMENTS:
C     MINN - The minimum Geiger value.
C     RANGE - The range of the Geiger values.
C     FRQARR - The array containing the frequencies of the Geiger values.
C     
C     PRECONDITION:
C     The args have already been set and are defined in the module calling
C     GRAPH.
C     POSTCONDITION:
C     A histogram of the Geiger values has been drawn.
C
C
      SUBROUTINE GRAPH(MINN, RANGE, NLINES, VALARR)
      INTEGER*4 MINN
      INTEGER*4 RANGE
      INTEGER*4 NLINES
      INTEGER*4 VALARR(NLINES)

      INTEGER*4 FRQARR(RANGE)
      INTEGER*4 MAXHT
      
      WRITE(*,*) 'How tall would you like your histogram to be?'
      READ(*,*) MAXHT

      CALL SETFRQ(NLINES, VALARR, RANGE, FRQARR)
      
      CALL DRAW(MINN, RANGE, FRQARR, MAXHT)

      RETURN
      END

C
C DRAW
C     DESCRIPTION: 
C     Creates a 2D array GRID with dimensions RANGE by MAXHT, which it fills
C     with the scaled data in the shape of a histogram (for each value of
c     RANGE, set SCALE*(frequency of that value) number of elements to some
C     character.
C     
C     ARGUMENTS:
C     MINN - The minimum Geiger value.
C     RANGE - The range of the Geiger values.
C     FRQARR - The frequency array.
C     MAXHT - The maximum height of the histogram.
C     
C     PRECONDITION:
C     Being called from subroutine GRAPH.
C     POSTCONDITION:
C     A histogram of the Geiger values has been drawn.
C
C
      SUBROUTINE DRAW(MINN, RANGE, FRQARR, MAXHT)
C     Arguments
      INTEGER*4 MINN
      INTEGER*4 RANGE
      INTEGER*4 FRQARR(RANGE)
      INTEGER*4 MAXHT

C     Local variables
      INTEGER*4 I
      INTEGER*4 J
      REAL*4 SCALE
      INTEGER*4 HOLDER
      CHARACTER*1 GRID(RANGE, MAXHT)

C     Functions
      INTEGER*4 FINDMD

      GRID=" "
      HOLDER=0

      SCALE = REAL(MAXHT)/REAL(FINDMD(RANGE, FRQARR))

CCCC  Setting the 2D array GRID
      DO 190 I=1, RANGE
         HOLDER = INT(SCALE*FRQARR(I))
         DO 200 J=1, HOLDER
            GRID(I, J) = "#"
200      CONTINUE            
190   CONTINUE

CCCC  Printing GRID
      DO 210 I=MAXHT, 1, -1
         DO 220 J=1, RANGE
            WRITE(*,3000) GRID(J, I)
3000        FORMAT(1A, $)
220      CONTINUE
         WRITE(*,*)
210   CONTINUE

      RETURN
      END


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C FUNCTIONS
C
C
C LINECT
C     DESCRIPTION:
C     Reads each line until it reaches the end of the text file,
C     adding to a local variable NUMLNS each time.
C
C     ARGUMENTS:
C     UNITN - The unit number assigned to the text file.
C
C     RETURN:
C     The number of lines in the text file.
C
C
      FUNCTION LINECT(UNITN)
      INTEGER*4 LINECT
      INTEGER*4 UNITN

      CHARACTER*128 LINE

      LINECT = 0
100   READ(UNITN, *, END=110)  LINE
         LINECT = LINECT + 1
         GOTO 100
110   CONTINUE

      RETURN
      END

C
C FINDMN
C     DESCRIPTION: 
C     Sets the main program's local variable MEAN. By traversing VALARR and
C     taking its total sum, then dividing that sum by NLINES, which is equal
C     to the number of data values.
C     
C     ARGUMENTS:
C     UNITN - The unit number assigned to the text file.
C     NLINES - The number of lines in the text file.
C     VALARR - The array containing all the Geiger values.
C     
C
      FUNCTION FINDMN(NLINES, VALARR)
      REAL*4 FINDMN
      INTEGER*4 NLINES
      INTEGER*4 VALARR(NLINES)

      INTEGER*4 I

      FINDMN = 0
      DO 160 I=1, NLINES
         FINDMN=FINDMN + REAL(VALARR(I))
160   CONTINUE
      FINDMN=FINDMN/REAL(NLINES)

      RETURN
      END

C
C FINDST
C     DESCRIPTION: 
C     Sets the main program's local variable STDV. Traverses VALARR and
C     performs the necessary calculations to the find standard deviation
C     of its contents.
C     
C     ARGUMENTS:
C     NLINES - The number of lines in the text file.
C     VALARR - The array containing all the Geiger values.
C     MEAN - The mean average of all the Geiger values.
C     
C
      FUNCTION FINDST(NLINES, VALARR, MEAN)
      REAL*4 FINDST
      INTEGER*4 NLINES
      INTEGER*4 VALARR(NLINES)
      REAL*4 MEAN

      INTEGER*4 I

      REAL*4 FSTDV

      FINDST = 0
      DO 170 I=1, NLINES
         FINDST = FINDST + (REAL(VALARR(I)) - MEAN)**2
170   CONTINUE
      FINDST = FINDST/REAL(NLINES)
      FINDST = SQRT(FINDST)

      RETURN
      END

C
C FINDMD
C     DESCRIPTION: 
C     Finds the mode of the Geiger values. Traverse FRQARR and checks whether
C     each value is greater than the current MAXX.
C     
C     ARGUMENTS:
C     RANGE - The range of the Geiger values.
C     FRQARR- The array with the frequencies of the Geiger calues.
C     
C
      FUNCTION FINDMD(RANGE,FRQARR)
      INTEGER*4 FINDMD
      INTEGER*4 RANGE
      INTEGER*4 FRQARR(RANGE)

      INTEGER*4 I
      INTEGER*4 FFQMAX

      FINDMD = FRQARR(1)
      DO 240 I=2, RANGE
         IF(FRQARR(I)>FINDMD) FINDMD=FRQARR(I)
240   CONTINUE

      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C BLOCK DATA
C
