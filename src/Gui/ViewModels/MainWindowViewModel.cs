using System.IO;
using Avalonia.Controls;
using Gui.Views;

namespace Gui.ViewModels
{

    public class MainWindowViewModel : ViewModelBase
    {
        public async void Open()
        {
            var dialog = new OpenFileDialog
            {
                InitialFileName = string.Empty
            };
            dialog.Filters.Add(new FileDialogFilter { Extensions = { "txt" } });
            var result = await dialog.ShowAsync(MainWindow.main);
            if (result is {Length: > 0})
            {
                MainWindow.CodeInput.Text = await File.ReadAllTextAsync(result[0]);
            }

            if (result != null) MainWindow.Filename = result[0];
        }
        
        public async void Save()
        {
            if (!string.IsNullOrEmpty(MainWindow.Filename))
            {
                await File.WriteAllTextAsync(MainWindow.Filename, MainWindow.CodeInput.Text);
            }
            else
            {
                var dialog = new SaveFileDialog
                {
                    InitialFileName = MainWindow.Filename
                };
                var result = await dialog.ShowAsync(MainWindow.main);
                if (result != null)
                {
                    await File.WriteAllTextAsync(result, MainWindow.CodeInput.Text);
                }
            }

            MainWindow.StatusBar.Text = "Changes saved to " + Path.GetFileName(MainWindow.Filename);    
            
        }
        
        public async void New()
        {
            var dialog = new SaveFileDialog();
            var result = await dialog.ShowAsync(MainWindow.main);
            if (result != null)
            {
                await File.WriteAllTextAsync(result, MainWindow.CodeInput.Text);
            }
            MainWindow.Filename = result;
        }
        
    }
}
