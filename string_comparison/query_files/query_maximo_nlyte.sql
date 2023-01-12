SELECT first_query.*
FROM (
        SELECT DISTINCT a.AssetID,
            a.AssetName,
            a.SerialNumber,
            manu.Detail,
            m.MaterialName,
            mType.Detail as MaterialType,
            matSType.Name as MaterialSubType,
            ROUND(kw.MpafPowerConsumption * 3 / 2, 3) as KW_Load,
            a2.AssetName as Parent_Asset_Name,
            u.UDisplayLabel as Cabinet_U_Number,
            CASE
                WHEN u.ZOffset < -50 THEN 'Back'
                ELSE 'Front'
            END as Mount,
            bg.BusinessGroupName,
            lg.LocationGroupName,
            SUBSTRING(
                lg.DuplicateLocationGroupName,
                CHARINDEX('(', lg.DuplicateLocationGroupName) + 1,
                CHARINDEX(',', lg.DuplicateLocationGroupName) - CHARINDEX('(', lg.DuplicateLocationGroupName) -1
            ) as City,
            rs.Description,
            a.CreationDate
        FROM Asset as a -- Location Group Join
            LEFT JOIN LocationGroup as lg ON a.LocationGroupID = lg.LocationGroupID -- Record Status Join
            LEFT JOIN RecordStatus as rs ON a.RecordStatus = rs.RecordStatusID -- Material Join
            LEFT JOIN Material as m ON a.MaterialID = m.MaterialID -- Material SubJoins
            -- Manufacturer Join
            LEFT JOIN Manufacturer as manu ON m.ManufacturerID = manu.ManufacturerID -- Material Type Join
            LEFT JOIN MaterialType as mType ON m.MaterialTypeID = mType.MaterialTypeID -- Material SubType Join
            LEFT JOIN MaterialSubtype as matSType ON m.MaterialSubtypeID = matSType.MaterialSubtypeID -- KVA
            LEFT JOIN MPafAssetView as kw ON a.AssetID = kw.AssetId -- Cabinet
            LEFT JOIN Asset as a2 ON a.CabinetAssetID = a2.AssetID -- UMounting
            LEFT JOIN UMounting as u ON a.UMountingID = u.UMountingID -- BusinessGroup
            LEFT JOIN BusinessGroupAssetMap as bgM ON bgM.AssetID = a.AssetID
            LEFT JOIN BusinessGroup as bg ON bgM.BusinessGroupID = bg.BusinessGroupID
        WHERE lg.DuplicateLocationGroupName LIKE '%BR-%' -- selecting all asset from Brazil
            AND a.RecordStatus NOT IN (1, 5) --removing planned and cancelled
            AND matSType.Name NOT IN ('Panel', 'Network Card', 'Bus Bar')
            AND mType.Detail NOT IN (
                'StaticSwitch',
                'UPS',
                'Powerstrip',
                'PDU',
                'Receptacle',
                'Cabinet'
            )
    ) as first_query
WHERE first_query.City LIKE '%HORT%'