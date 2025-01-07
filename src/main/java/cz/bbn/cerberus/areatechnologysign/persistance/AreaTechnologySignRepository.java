package cz.bbn.cerberus.areatechnologysign.persistance;

import cz.bbn.cerberus.area.persistance.entity.AreaEntity;
import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.technology.persistance.entity.TechnologyEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;

import java.util.List;

public interface AreaTechnologySignRepository extends JpaRepository<AreaTechnologySignEntity, Long> {


    boolean existsByObjectTypeAndObjectIdAndAreaEntityAndTechnologyEntity(ObjectType objectType, String objectId,
                                                                          AreaEntity areaEntity, TechnologyEntity technologyEntity);

    List<AreaTechnologySignEntity> findByObjectTypeAndObjectId(ObjectType objectType, String objectId);

    @Modifying
    void deleteByObjectTypeAndObjectId(ObjectType objectType, String objectId);

    @Modifying
    void deleteByAreaEntityId(String areaId);

    @Modifying
    void deleteByTechnologyEntityId(String areaId);

}
