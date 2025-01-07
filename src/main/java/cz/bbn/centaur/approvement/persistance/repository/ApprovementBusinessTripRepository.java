package cz.bbn.cerberus.approvement.persistance.repository;

import cz.bbn.cerberus.approvement.persistance.entity.ApprovementBusinessTripEntity;
import org.springframework.data.jpa.repository.JpaRepository;

public interface ApprovementBusinessTripRepository extends JpaRepository<ApprovementBusinessTripEntity, Long> {
}
