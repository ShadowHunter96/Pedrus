package cz.bbn.cerberus.activity.dto;

import cz.bbn.cerberus.activity.persistance.ActivityByObjectId;
import cz.bbn.cerberus.enumeration.dto.EnumerationDto;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;


@Getter
@Setter
@NoArgsConstructor
public class ActivityByObjectDto {

    private ActivityByObjectId id;
    private EnumerationDto enumerationDto;

    public ActivityByObjectDto(ActivityByObjectId id) {
        this.id = id;
    }
}
