SELECT DISTINCTROW Structures.[Structure Name],
       Functions.[Function Name],
       [Structure Functions].ID
FROM (Functions INNER JOIN (Structures INNER JOIN [Structure Functions] ON Structures.StructureID = [Structure Functions].StructureID) ON Functions.[Function ID] = [Structure Functions].[Function ID]) INNER JOIN [Technology Type] ON Structures.[TechnologyType ID] = [Technology Type].[TechnologyType ID]
WHERE ((([Technology Type].[Tecnology Name])="Level Three" Or ([Technology Type].[Tecnology Name])="Level Two-Three" Or ([Technology Type].[Tecnology Name])="Level All") AND ((Structures.psx)=Yes));
