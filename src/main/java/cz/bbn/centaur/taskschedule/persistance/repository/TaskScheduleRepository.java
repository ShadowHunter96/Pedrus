package cz.bbn.cerberus.taskschedule.persistance.repository;

import cz.bbn.cerberus.taskschedule.persistance.entity.TaskScheduleEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;

public interface TaskScheduleRepository extends JpaRepository<TaskScheduleEntity, Long>,
        JpaSpecificationExecutor<TaskScheduleEntity> {
}
