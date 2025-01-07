package cz.bbn.cerberus.management.ui;

import com.vaadin.flow.component.datepicker.DatePicker;
import com.vaadin.flow.component.html.H2;
import com.vaadin.flow.component.orderedlayout.FlexLayout;
import com.vaadin.flow.component.radiobutton.RadioButtonGroup;
import com.vaadin.flow.router.Route;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.commons.component.ui.AppView;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.appcard.AppCard;
import cz.bbn.cerberus.commons.component.ui.appcard.AppNumberCard;
import cz.bbn.cerberus.commons.component.ui.appcard.AppStringCard;
import cz.bbn.cerberus.commons.component.ui.layouts.CenteredHorizontalLayout;
import cz.bbn.cerberus.commons.component.ui.slidetab.NewEntityButtonsComponent;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.commons.enums.QuarterDateFilter;
import cz.bbn.cerberus.employee.ui.EmployeeView;
import cz.bbn.cerberus.mainlayout.ui.MainLayout;
import cz.bbn.cerberus.management.ManagementComponentOperation;
import cz.bbn.cerberus.note.NoteTypeEnum;
import cz.bbn.cerberus.permission.Authorize;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.translation.Transl;
import lombok.extern.slf4j.Slf4j;

import java.time.LocalDate;

@Route(value = ManagementIntranetDashboardView.ROUTE, layout = MainLayout.class)
@Authorize(Permission.MANAGEMENT_DASHBOARD)
@Slf4j
public class ManagementIntranetDashboardView extends AppView {

    public static final String ROUTE = "management-intranet-dashboard";

    private final EntityNewComponentOperation entityNewComponentOperation;
    private final ManagementComponentOperation managementComponentOperation;

    public ManagementIntranetDashboardView(EntityNewComponentOperation entityNewComponentOperation,
                                           ManagementComponentOperation managementComponentOperation) {
        this.entityNewComponentOperation = entityNewComponentOperation;
        this.managementComponentOperation = managementComponentOperation;
        initTab();
    }

    private void initTab() {
        setSizeFull();
        AppCard card = new AppCard(entityNewComponentOperation, NoteTypeEnum.ANY, ObjectType.ANY);
        card.addNewEntitySlideTab(new NewEntityButtonsComponent(entityNewComponentOperation));
        card.addToHeader(new H2(Transl.get("Intranet dashboard")));
        QuarterDateFilter value = AppUtils.getQuarterDateFilterValue(LocalDate.now());
        resetLayout(card, value);
        add(card);
    }

    private void resetLayout(AppCard card, QuarterDateFilter value) {
        card.getContent().removeAll();

        CenteredHorizontalLayout centeredHorizontalLayout = new CenteredHorizontalLayout();
        FlexLayout countFlexLayout = new FlexLayout();

        countFlexLayout.setJustifyContentMode(JustifyContentMode.CENTER);

        CenteredHorizontalLayout centeredCircleGraphHorizontalLayout = new CenteredHorizontalLayout();
        countFlexLayout.setFlexWrap(FlexLayout.FlexWrap.WRAP);
        countFlexLayout.addClassName("flex-layout");
        centeredCircleGraphHorizontalLayout.add(countFlexLayout);

        DatePicker from = VaadinComponents.getDatePicker(Transl.get("From"), null);
        DatePicker to = VaadinComponents.getDatePicker(Transl.get("To"), null);
        AppUtils.setDatesByQuarterFilter(value, from, to);
        from.setMax(to.getValue());
        to.setMin(from.getValue());

        RadioButtonGroup<QuarterDateFilter> quarterDateFilter = new RadioButtonGroup<>();
        quarterDateFilter.setItems(QuarterDateFilter.values());
        quarterDateFilter.setItemLabelGenerator(actual -> Transl.get(actual.name()));
        quarterDateFilter.addValueChangeListener(event -> {
            AppUtils.setDatesByQuarterFilter(event.getValue(), from, to);
            if (event.isFromClient() && !event.getValue().equals(QuarterDateFilter.OWN_FILTER)) {
                resetLayout(card, event.getValue());
            }
        });
        quarterDateFilter.setValue(value);

        from.addValueChangeListener(event -> {
            if (event.getValue() != null && to.getValue() != null) {
                managementComponentOperation.reloadData(countFlexLayout);
                if (event.isFromClient()) {
                    to.setMin(from.getValue());
                    quarterDateFilter.setValue(QuarterDateFilter.OWN_FILTER);
                }
            }
        });

        to.addValueChangeListener(event -> {
            if (event.getValue() != null && from.getValue() != null) {
                managementComponentOperation.reloadData(countFlexLayout);
                if (event.isFromClient()) {
                    from.setMax(to.getValue());
                    quarterDateFilter.setValue(QuarterDateFilter.OWN_FILTER);
                }
            }
        });
        centeredHorizontalLayout.add(from, to);
        addAppNumberCards(countFlexLayout, from, to);

        CenteredHorizontalLayout radioGroupLayout = new CenteredHorizontalLayout();
        radioGroupLayout.add(quarterDateFilter);

        card.add(centeredHorizontalLayout, radioGroupLayout, centeredCircleGraphHorizontalLayout);
    }

    private void addAppNumberCards(FlexLayout flexCountLayout, DatePicker from, DatePicker to) {
            flexCountLayout.add(new AppNumberCard(Transl.get("Number of employees"),
                    managementComponentOperation.getEmployeeCount(from, to), EmployeeView.ROUTE, "19em"));
            flexCountLayout.add(new AppStringCard(Transl.get("Reported hours"),
                    managementComponentOperation.getReportedHours(from, to), null, "19em"));
            flexCountLayout.add(new AppNumberCard(Transl.get("Required hours"),
                    managementComponentOperation.getRequiredHours(from, to), "", "19em"));
    }

}
