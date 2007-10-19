SELECT Component.[Component Name],
       [Technology Type].[Tecnology Name],
       BodySIze.[Body Size],
       Component.[Power Required],
       Component.[Build Points Required],
       Component.Weight,
       Component.[Body Points],
       Component.[Graphics filename0],
       Body.[Max System Points],
       Body.[Weapon Slots],
       Body.[Power Output],
       Body.[Kinetic Armour Value],
       Body.[HeatArmour Value],
       Body.[Flame IMD],
       Body.design
FROM [Technology Type] INNER JOIN ((Component INNER JOIN Body ON Component.[Component ID] = Body.[Component ID]) INNER JOIN BodySIze ON Body.Size = BodySIze.id) ON [Technology Type].[TechnologyType ID] = Component.[TechnologyType ID]
ORDER BY Component.[Component Name] DESC;
