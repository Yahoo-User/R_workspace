1. How to check the encoding of a file ?

    $ file -bi <filename>

    $ file -bi *


2. How to change the encoding of file ?

    $ iconv -f <source_encoding> -t <target_encoding> -o <target_file> <source_file>


3. How to change the encodings of all files ?

    $ iconv -l

    $ iconv -l | grep ISO-8859-1
    ISO-8859-1//

    $ iconv -l | grep 949
    CP949//
    MSCP949//

    $ file -bi <filename>
    application/csv; charset=iso-8859-1


    $ head <filename> 
    ID,�����,����,���,������ũ,��,�ſ���,������,�籸���ǻ�
    1,0,1,0,0,0,1,1,1
    ...

    $ iconv -f CP949 <filename> | head
    ID,자장면,피자,통닭,스테이크,립,매운탕,육개장,재구매의사
    1,0,1,0,0,0,1,1,1
    ...

    $ iconv -f CP949 -o <filename> <filename>
    $ file -bi <filename>


    $ for fn in `ls ch*.csv`;
    do
        iconv -f CP949 -o ${fn} ${fn}
    done


    $ file -bi <filename>
