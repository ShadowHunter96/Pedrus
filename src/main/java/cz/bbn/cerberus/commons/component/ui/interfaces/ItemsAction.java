package cz.bbn.cerberus.commons.component.ui.interfaces;

import com.vaadin.flow.data.provider.Query;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Sort;

import java.util.List;

public interface ItemsAction<T> {
    Page<T> getItems(Query<T, Void> query, List<Sort.Order> orderList);
}
