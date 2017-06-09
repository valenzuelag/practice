       identification division. 
       program-id. TOTALSLE.
       author. Gabriel Valenzuela and Mark Holada.
       date-written. 4/5/2017
      ******************************************************************
      *Purpose: To allow a user to enter the desired amount of records
      *which will compute the final sale for all the items purchased
      *by the cusomter. 
      *
      *Input File: N/A
      *           
      *    
      *The output is a file that gives the customer's information
      *along with the price of the unit, the amount sold and returned,
      *the total sale, sales tax, and final sale for the customer. It 
      *also creates a report confirmaing which records were entered
      *from the user.     
      *
      *Date/Time Due: April 13, 2017 at 12:00 PM
      *Data Files: SALES-RPT "J:\COBOL\sales.doc"
      *            SALES-FILE "J:\COBOL\sales.dat"
      *            
      ******************************************************************
       environment division. 
       input-output section.
       file-control. 
           select SALES-RPT
               assign to UT-SYS-SALESRPT
               organization is line sequential.
           select SALES-FILE
               assign to UT-SYS-SALESFILE
               organization is sequential.    
       data division.
       file section.
       FD SALES-FILE
           RECORD CONTAINS 67.
       01 CUST-REC.
           05 CUST-NO                PIC X(4).
           05 CUST-NAME              PIC X(25).
           05 CUST-UNT-PRICE         PIC 9999V99.
           05                        PIC XXXX. 
           05 CUST-QNTY-RTND         PIC S9999.
           05                        PIC X(7).
           05 CUST-TSALE             PIC S9999V99.
           05 CUST-STAX              PIC S999V99.
           05 CUST-FSALE             PIC S9999V99.
            
       FD SALES-RPT
           RECORD CONTAINS 65.
       01 SALES-REC                  PIC X(65).
       
       working-storage section. 
       01 WS-VARS.
           05 WS-TOTAL-SALE          PIC S9999V99.
           05 WS-FINAL-SALE          PIC S9999V99.
           05 WS-SALES-TAX           PIC S999V99.
           05 WS-PGNO                PIC 9(3) VALUE 1. 
           05 WS-UNIT-PRICE          PIC S9999V99.
           05 WS-LINECT              PIC 99.
           05 WS-ADD-INPUT           PIC XXX VALUE "NO".
           05 WS-EOU                 PIC XXXX VALUE "YES".
           05 WS-HEADERADV           PIC 9 VALUE 7.
       01 WS-FILES.
           05 UT-SYS-SALESFILE       PIC X(67)
              VALUE "J:\COBOL\sales.dat".
           05 UT-SYS-SALESRPT        PIC X(65)
              VALUE "J:\COBOL\sales.doc".
       01 WS-CONSTANTS.
           05 WS-FULL-PAGE           PIC 99 VALUE 61.
           05 WS-TAX                 PIC V999 VALUE .065.
       01 WS-DATE.
           05 WS-YEAR                PIC 9999.
           05 WS-MONTH               PIC 99.
           05 WS-DAY                 PIC 99.
       01 HEADING1.
           05 H-MONTH                PIC 99.
           05                        PIC X VALUE "/".
           05 H-DAY                  PIC 99.
           05                        PIC X VALUE "/".
           05 H-YEAR                 PIC 9999.
           05                        PIC X(12) VALUE SPACES.
           05                        PIC X(35) 
                                     VALUE "FINAL PROJECT COMPANY".
           05                        PIC X(5) VALUE "PAGE".
           05 H-PAGENO               PIC ZZ9.
       01 HEADING2.
           05                        PIC X(47) 
                            VALUE "GABRIEL VALENZUELA AND MARK HOLADA".
       
           05                        PIC X(18) VALUE " SALES REPORT".
       01 HEADING3.
           05                        PIC X(7) VALUE "CUST NO".
           05                        PIC X(27) VALUE "CUST NAME".
           05                        PIC X(31) VALUE "RECORD ADDED".
       01 HEADING4.
           05                        PIC X(7) VALUE " NO".
           05                        PIC X(29) VALUE "NAME".
           05                        PIC X(29) VALUE "(YES/NO)".
       01 HEADINGBLANK               PIC X VALUE SPACE.
       01 DETAIL1.
           05 D-NO                   PIC X(5).
           05                        PIC XX VALUE "  ".
           05 D-NAME                 PIC X(24).
           05                        PIC X(6) VALUE SPACES.
           05 D-Y-N                  PIC XXX.
       PROCEDURE DIVISION.
      ******************************************************************
      *100-MAIN Perform initial tasks, write our headers on first page
      *         add the record from the user. Loop until Stop. Close the
      *         files and stop the program. 
      ******************************************************************       
       100-MAIN.
           PERFORM 110-INIT THRU 110-EXIT
           PERFORM 600-WRITE-HEADER THRU 600-EXIT
           PERFORM 400-ADD-RECORD THRU 400-EXIT
                UNTIL FUNCTION UPPER-CASE(WS-EOU) = "STOP"
           CLOSE SALES-FILE
                 SALES-RPT
       STOP RUN.
      ******************************************************************
      *110-INIT Opens the outputs and gets the current date.
      ******************************************************************
       110-INIT.
            OPEN OUTPUT SALES-FILE
                        SALES-RPT
            MOVE FUNCTION CURRENT-DATE TO WS-DATE
            MOVE WS-MONTH TO H-MONTH
            MOVE WS-DAY TO H-DAY
            MOVE WS-YEAR TO H-YEAR.
       110-EXIT.
           EXIT.
   

      ******************************************************************
      *800-INIT-USER-INPUT Asks the user the customer's number, name, 
      *                    a positive unit price, and either the amount 
      *                    sold with a positive number or returned with 
      *                    a negative number.
      ******************************************************************
       800-INIT-USER-INPUT.
           DISPLAY "Enter Customer Number"
           ACCEPT CUST-NO
           DISPLAY "Enter Customer Name"
           ACCEPT CUST-NAME
           DISPLAY "Enter Unit Price"
           ACCEPT WS-UNIT-PRICE
           IF WS-UNIT-PRICE IS LESS THAN ZERO
              PERFORM 210-REPROMPT-PRICE THRU 210-EXIT
                      UNTIL WS-UNIT-PRICE > 0
           END-IF
           MOVE WS-UNIT-PRICE TO CUST-UNT-PRICE
           DISPLAY "Enter Quantity"
           ACCEPT CUST-QNTY-RTND.
       
       800-EXIT.
           EXIT.
      ******************************************************************
      *210-REPROMPT-PRICE Asks for the unit price until it is a 
      *                   positive number
      ******************************************************************
       210-REPROMPT-PRICE.
           DISPLAY "Enter Unit Price"
           ACCEPT WS-UNIT-PRICE.
       210-EXIT.
           EXIT. 
      
      ******************************************************************
      *300-COMPUTE-RECORD Computes the total sale, sales tax, and final
      *                   sale for each customer. If an on size error 
      *                   it will change the value computed to zero     
      ******************************************************************
       300-COMPUTE-RECORD.
           MULTIPLY CUST-UNT-PRICE BY CUST-QNTY-RTND
               GIVING WS-TOTAL-SALE
               ON SIZE ERROR PERFORM 700-ON-SIZE-PROBLEM
           END-MULTIPLY
           MULTIPLY WS-TOTAL-SALE BY WS-TAX
               GIVING WS-SALES-TAX
               ON SIZE ERROR PERFORM 700-ON-SIZE-PROBLEM
           END-MULTIPLY
           ADD WS-TOTAL-SALE TO WS-SALES-TAX
               GIVING WS-FINAL-SALE
               ON SIZE ERROR PERFORM 700-ON-SIZE-PROBLEM
           END-ADD.
       300-EXIT.
           EXIT.
      ******************************************************************
      *400-ADD-RECORD Asks for the user's input, computes the data 
      *               entered by the user, asks if the user wants to add
      *               the record. Asks if the user wants to stop or 
      *               continue adding records.               
      ******************************************************************
       400-ADD-RECORD.
           PERFORM 800-INIT-USER-INPUT THRU 800-EXIT
           PERFORM 300-COMPUTE-RECORD THRU 300-EXIT
           DISPLAY "Are you sure you want to add this record?"
           ACCEPT WS-ADD-INPUT
           IF FUNCTION UPPER-CASE(WS-ADD-INPUT) = "YES"
               MOVE FUNCTION UPPER-CASE(WS-ADD-INPUT) TO WS-ADD-INPUT
               PERFORM 500-WRITE-RECORD THRU 500-EXIT
               PERFORM 510-WRITE-DNO THRU 510-exit 
           ELSE
               IF FUNCTION UPPER-CASE(WS-ADD-INPUT) = "NO"
                   MOVE FUNCTION UPPER-CASE(WS-ADD-INPUT) TO
                   WS-ADD-INPUT
                   PERFORM 500-WRITE-RECORD THRU 500-EXIT
               ELSE 
                   IF WS-ADD-INPUT NOT EQUAL TO "YES" OR "NO"
                       PERFORM 410-REPROMPT-RECORD THRU 410-EXIT
                           UNTIL WS-ADD-INPUT = "NO" OR "YES"
                       
                   END-IF
               END-IF
           END-IF
           DISPLAY "ADD MORE RECORDS? STOP TO FINISH"
           ACCEPT WS-EOU.
       400-EXIT.
           EXIT.
      ******************************************************************
      *410-REPROMPT-RECORD Asks the user if they want to add a record
      *                    and will loop until the user enteres yes or 
      *                    no.
      ******************************************************************
       410-REPROMPT-RECORD.
           DISPLAY "Are you sure you want to add this record?"
           ACCEPT WS-ADD-INPUT
           IF FUNCTION UPPER-CASE(WS-ADD-INPUT) = "YES"
               MOVE FUNCTION UPPER-CASE(WS-ADD-INPUT) TO WS-ADD-INPUT
               PERFORM 500-WRITE-RECORD THRU 500-EXIT
               PERFORM 510-WRITE-DNO THRU 510-exit 
           ELSE
               IF FUNCTION UPPER-CASE(WS-ADD-INPUT) = "NO"
                   MOVE FUNCTION UPPER-CASE(WS-ADD-INPUT) TO
                   WS-ADD-INPUT
                   PERFORM 500-WRITE-RECORD THRU 500-EXIT
               END-IF
           END-IF.
       410-EXIT.
           EXIT. 
      ******************************************************************
      *500-WRITE-RECORD Moves the input data to the ouput data and also
      *                 displays if the record was entered or not. 
      *                 Writes the record to the report and increases 
      *                 the line count 
      ******************************************************************
       500-WRITE-RECORD.
           MOVE CUST-NO TO D-NO
           MOVE CUST-NAME TO D-NAME
           MOVE WS-ADD-INPUT TO D-Y-N 
           WRITE SALES-REC FROM DETAIL1
                AFTER ADVANCING 1 LINE 
           ADD 1 TO WS-LINECT
           IF WS-LINECT > WS-FULL-PAGE
              MOVE 0 TO WS-LINECT
              PERFORM 600-WRITE-HEADER THRU 600-EXIT
              ADD 1 TO WS-PGNO
           END-IF.
       500-EXIT.
           exit. 
      ******************************************************************
      *510-WRTIE-DNO Moves the input data to the ouput data with the tax
      *, total sale, and final sale and wrties the data to the file.
      ******************************************************************
       510-WRITE-DNO.
           MOVE WS-TOTAL-SALE TO CUST-TSALE
           MOVE WS-SALES-TAX TO CUST-STAX
           MOVE WS-FINAL-SALE TO CUST-FSALE
           WRITE CUST-REC.
       510-EXIT.
           exit.
      ******************************************************************
      *600-WRITE-HEADER Writes the headers for the report and increases
      *                 page number from where the heading starts.
      ******************************************************************
       600-WRITE-HEADER.
           MOVE WS-PGNO TO H-PAGENO
           WRITE SALES-REC FROM HEADING1
                 AFTER ADVANCING PAGE
           WRITE SALES-REC FROM HEADING2
                 AFTER ADVANCING 1 LINE
           WRITE SALES-REC FROM HEADINGBLANK
                 AFTER ADVANCING 2 LINES
           WRITE SALES-REC FROM HEADING3
                 AFTER ADVANCING 1 LINE
           WRITE SALES-REC FROM HEADING4
                 AFTER ADVANCING 1 LINE.
           WRITE SALES-REC FROM HEADINGBLANK
                 AFTER ADVANCING 1 LINE
           ADD WS-HEADERADV TO WS-LINECT.
       600-EXIT.
           EXIT.
      ******************************************************************
      *700-ON-SIZE-PROBLEM Moves zero to the value if an on size error 
      *                    occurs while computing. Tells the user that
      *                    the numbers entered were to large. 
      ******************************************************************
       700-ON-SIZE-PROBLEM.
           MOVE ZERO TO WS-TOTAL-SALE
           MOVE ZERO TO WS-SALES-TAX
           MOVE ZERO TO WS-FINAL-SALE
           DISPLAY "There was a problem with the size of your numbers,".
       700-EXIT.
           EXIT.