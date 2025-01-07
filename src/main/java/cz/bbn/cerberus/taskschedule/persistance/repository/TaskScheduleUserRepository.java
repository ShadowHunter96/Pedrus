package cz.bbn.cerberus.taskschedule.persistance.repository;

import cz.bbn.cerberus.taskschedule.persistance.entity.TaskScheduleUserEntity;
import cz.bbn.cerberus.taskschedule.persistance.entity.TaskScheduleUserId;
import org.springframework.data.jpa.repository.JpaRepository;

public interface TaskScheduleUserRepository extends JpaRepository<TaskScheduleUserEntity, TaskScheduleUserId> {

    void deleteByIdTaskScheduleEntityId(Long id);
}
