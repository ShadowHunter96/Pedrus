package cz.bbn.cerberus.applog.ui.components.tab;

import com.vaadin.flow.component.button.Button;
import cz.bbn.cerberus.commons.RobotFrameworkVariables;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.component.ui.tab.TabSimpleComponent;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.applog.ui.components.SchedulerLogFilterComponent;
import cz.bbn.cerberus.applog.ui.components.SchedulerLogGridComponent;
import cz.bbn.cerberus.schedulerlog.SchedulerLogService;
import cz.bbn.cerberus.schedulerlog.dto.SchedulerLogDto;
import cz.bbn.cerberus.schedulerlog.dto.SchedulerLogFilterDto;
import org.springframework.data.domain.Sort;

import java.util.List;

public class SchedulerLogTab extends TabSimpleComponent {

    public static final int TAB_INDEX = 1;

    private final AppEnv appEnv;
    private final SchedulerLogService schedulerLogService;

    public SchedulerLogTab(AppEnv appEnv, SchedulerLogService schedulerLogService) {
        this.appEnv = appEnv;
        this.schedulerLogService = schedulerLogService;
        initView();
    }

    private void initView() {
        removeAll();
        this.setId(RobotFrameworkVariables.SCHEDULER_LOG_TAB_ID.getValue());

        Button search = VaadinComponents.getSearchButton();
        SchedulerLogFilterComponent schedulerLogFilterComponent =
                new SchedulerLogFilterComponent(search, getDescriptionList());
        this.add(schedulerLogFilterComponent);

        SchedulerLogGridComponent schedulerLogGridComponent =
                new SchedulerLogGridComponent(appEnv, getItemsAction(schedulerLogFilterComponent));
        schedulerLogGridComponent.loadData();
        search.addClickListener(buttonClickEvent -> schedulerLogGridComponent.loadData());
        this.add(schedulerLogGridComponent);
        this.setSizeFull();
    }

    private ItemsAction<SchedulerLogDto> getItemsAction(SchedulerLogFilterComponent filter) {
        return (query, orderList) -> {
            SchedulerLogFilterDto filterDto = filter.getSchedulerLogFilterDto();
            filterDto.setPage(query.getPage());
            filterDto.setSize(query.getPageSize());
            if (orderList.isEmpty()) {
                orderList.add(Sort.Order.desc("date"));
            }
            filterDto.setOrderList(orderList);
            return schedulerLogService.findSchedulerLogDtoPage(filterDto);
        };
    }

    private List<String> getDescriptionList() {
        return schedulerLogService.getDescriptionList();
    }
}
