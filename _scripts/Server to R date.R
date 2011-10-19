library("RODBC")

DServer <- odbcDriverConnect(connection = "DRIVER=SQL Server;Trusted_Connction=Yes;DATABASE=ViCo;SERVER=FSX-GT3")

resp <- sqlQuery(DServer, "SELECT * FROM ViCo.Clinicos.Basica_Respira where FechaHoraAdmision < '2011-10-16'")

