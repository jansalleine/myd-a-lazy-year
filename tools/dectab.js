let string = "";

for ( let i = 0; i < 100; i++ )
{
    if ( !(i % 8) )
        string += "\n!byte ";
    string += "$";
    string += ( "00" + i).slice(-2);
    if ( ( i % 8 ) < 7 && i < 99 )
        string +=", ";
}

console.log( string );
