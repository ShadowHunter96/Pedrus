package cz.bbn.cerberus.activity.persistance;

import cz.bbn.cerberus.enumeration.persistance.entity.EnumerationEntity;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;

import javax.persistence.EmbeddedId;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.OneToOne;
import javax.persistence.Table;

@Entity
@Table(name = "activity_by_object", schema = "enums")
@Getter
@Setter
@ToString
@NoArgsConstructor
public class ActivityByObjectEntity {

    @EmbeddedId
    private ActivityByObjectId id;

    @OneToOne
    @JoinColumn(name = "activity_id", referencedColumnName = "id", insertable = false, updatable = false)
    private EnumerationEntity enumerationEntity;

    public ActivityByObjectEntity(ActivityByObjectId id) {
        this.id = id;
    }
}
