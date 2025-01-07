package cz.bbn.cerberus.commons.component.ui.breadcrump;

import com.vaadin.flow.component.icon.VaadinIcon;
import cz.bbn.cerberus.commons.AppUtils;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@AllArgsConstructor
public class Breadcrumb {
    private String href;
    private String text;
    private String fullText;
    private String title;
    private VaadinIcon icon;

    public Breadcrumb(String href, String text, VaadinIcon icon) {
        this.text = AppUtils.cutText(text, 16, true);
        this.fullText = text;
        this.href = href;
        this.icon = icon;
        this.title = text + " -> " + href;
    }
}
