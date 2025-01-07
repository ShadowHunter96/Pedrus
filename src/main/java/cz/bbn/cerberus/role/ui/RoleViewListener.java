package cz.bbn.cerberus.role.ui;

import cz.bbn.cerberus.role.persistance.entity.RoleEntity;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Sort;

import java.util.List;

public interface RoleViewListener {

    Page<RoleEntity> getPage(int page, int size, List<Sort.Order> orderList);
}
