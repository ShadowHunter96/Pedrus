package cz.bbn.cerberus.activity.dto;

import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.enumeration.dto.EnumerationDto;
import lombok.Getter;
import lombok.Setter;

import java.util.Set;

@Getter
@Setter
public class ActivityLinkDto {
    private String objectId;
    private ObjectType objectType;
    private Set<EnumerationDto> activityDtoSet;
}
