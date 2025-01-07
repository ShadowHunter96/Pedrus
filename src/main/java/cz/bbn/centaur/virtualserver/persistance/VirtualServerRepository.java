package cz.bbn.cerberus.virtualserver.persistance;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;

import java.util.Set;

public interface VirtualServerRepository extends JpaRepository<VirtualServerEntity, Long>,
        JpaSpecificationExecutor<VirtualServerEntity> {

    @Query("select entity from VirtualServerEntity entity where entity.stringId = null")
    Set<VirtualServerEntity> getWithoutId();
}
