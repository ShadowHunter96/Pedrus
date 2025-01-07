package cz.bbn.cerberus.schedulerlog.persistance;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;

import java.util.List;

public interface SchedulerLogRepository
        extends JpaRepository<SchedulerLogEntity, Long>, JpaSpecificationExecutor<SchedulerLogEntity> {

    @Query("select distinct entity.description " +
            "from SchedulerLogEntity entity")
    List<String> getDescriptionList();
}
