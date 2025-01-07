package cz.bbn.cerberus.attendance.ui.component;

import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.combobox.ComboBox;
import cz.bbn.cerberus.attendance.ui.AttendanceReportView;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.tab.TabEntry;
import cz.bbn.cerberus.commons.component.ui.tab.TabSimpleComponent;
import cz.bbn.cerberus.commons.component.ui.tab.TabsComponent;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.workreport.dto.YearMonthDto;

import java.util.List;

public class AttendanceTabsComponent extends TabsComponent<TabSimpleComponent> {

    public AttendanceTabsComponent(String title, List<TabEntry> list, int activeTab,
                                   EntityNewComponentOperation entityNewComponentOperation,
                                   ComboBox<YearMonthDto> yearMonthDtoComboBox) {
        super(title, list, activeTab, null, entityNewComponentOperation);
        getContent().setMargin(false);
        Button report = VaadinComponents.getNewButton(Transl.get("Generate report"));
        report.addClickListener(buttonClickEvent ->
                UI.getCurrent().navigate(AttendanceReportView.ROUTE .concat("/").concat("&month=").concat(yearMonthDtoComboBox.getValue().getYearAndMonth())));
        this.addToButtonFooter(report);
    }
}
