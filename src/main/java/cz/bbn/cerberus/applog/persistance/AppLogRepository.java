package cz.bbn.cerberus.applog.persistance;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;

import java.util.List;

public interface AppLogRepository extends JpaRepository<AppLogEntity, Long>, JpaSpecificationExecutor<AppLogEntity> {

    @Query("select entity.action from AppLogEntity entity group by entity.action")
    List<String> getActionList();

}
