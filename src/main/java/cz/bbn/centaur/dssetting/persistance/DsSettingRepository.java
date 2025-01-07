package cz.bbn.cerberus.dssetting.persistance;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.List;

public interface DsSettingRepository extends JpaRepository<DsSettingEntity, String> {

    @Query("select entity.id from DsSettingEntity entity where entity.deleted = false")
    List<String> findAllId();
}
