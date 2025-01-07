package cz.bbn.cerberus.activity.persistance;

import cz.bbn.cerberus.commons.enums.ObjectType;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;

import javax.persistence.Column;
import javax.persistence.Embeddable;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import java.io.Serializable;

@Embeddable
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@ToString
public class ActivityByObjectId implements Serializable {

    @Column(name = "activity_id")
    private Long activityId;

    @Column(name = "object_id")
    private String objectId;

    @Column(name = "object_type")
    @Enumerated(EnumType.STRING)
    private ObjectType objectType;
}
