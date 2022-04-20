with import <nixpkgs> { };
stdenv.mkDerivation {
  name = "fpco-odbc";
  buildInputs = [
    unixODBC
    unixODBCDrivers.msodbcsql17
    ghc
  ];

  # Modified from  https://gist.github.com/knedlsepp/31f38110286fe244c6f8473e065f68cf
  shellHook = let
    ODBCINI = writeTextDir "odbcinst.ini" ''
      [ODBC Driver 17 for SQL Server]
      Description=Microsoft ODBC Driver 17 for SQL Server
      Driver=${unixODBCDrivers.msodbcsql17}/${unixODBCDrivers.msodbcsql17.passthru.driver}
      UsageCount=1
    '';
   in ''
    export ODBCSYSINI=${ODBCINI}
    export ODBC_TEST_CONNECTION_STRING='DRIVER={ODBC Driver 17 for SQL Server};SERVER=127.0.0.1,8090;Uid=SA;Pwd=Passw0rd;Encrypt=no'
   '';
}
