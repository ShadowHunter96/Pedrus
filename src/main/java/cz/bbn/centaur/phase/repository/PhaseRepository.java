package cz.bbn.cerberus.phase.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;

public interface PhaseRepository extends JpaRepository<PhaseEntity, Long>, JpaSpecificationExecutor<PhaseEntity> {
}
