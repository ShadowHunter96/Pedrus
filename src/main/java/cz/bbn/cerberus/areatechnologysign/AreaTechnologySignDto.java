package cz.bbn.cerberus.areatechnologysign;


import cz.bbn.cerberus.area.dto.AreaDto;
import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.technology.dto.TechnologyDto;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.io.Serializable;


@Getter
@Setter
@NoArgsConstructor
public class AreaTechnologySignDto implements Serializable {

    private Long id;
    private String objectId;
    private ObjectType objectType;
    private AreaDto areaDto;
    private TechnologyDto technologyDto;

}
