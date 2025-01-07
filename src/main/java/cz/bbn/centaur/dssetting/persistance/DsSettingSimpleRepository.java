package cz.bbn.cerberus.dssetting.persistance;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;

public interface DsSettingSimpleRepository
        extends JpaRepository<DsSettingSimpleEntity, String>, JpaSpecificationExecutor<DsSettingSimpleEntity> {
}
