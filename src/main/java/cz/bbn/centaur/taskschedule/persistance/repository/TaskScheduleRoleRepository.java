package cz.bbn.cerberus.taskschedule.persistance.repository;

import cz.bbn.cerberus.taskschedule.persistance.entity.TaskScheduleRoleEntity;
import cz.bbn.cerberus.taskschedule.persistance.entity.TaskScheduleRoleId;
import org.springframework.data.jpa.repository.JpaRepository;

public interface TaskScheduleRoleRepository extends JpaRepository<TaskScheduleRoleEntity, TaskScheduleRoleId> {

    void deleteByIdTaskScheduleEntityId(Long id);
}
