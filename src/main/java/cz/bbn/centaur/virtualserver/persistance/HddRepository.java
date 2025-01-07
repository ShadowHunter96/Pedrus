package cz.bbn.cerberus.virtualserver.persistance;

import org.springframework.data.jpa.repository.JpaRepository;

public interface HddRepository extends JpaRepository<HddEntity, Long> {
}
